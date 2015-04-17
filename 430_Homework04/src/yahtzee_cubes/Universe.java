package yahtzee_cubes;

import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

/**
 * Erlang version of the Universe in which cubes are implemented as Erlang
 * processes. In addition to starting up a Swing UI, this code starts
 * an Erlang node and an associated mailbox. Cubes are identified by their
 * Erlang pid. Cubes can send four types of messages to the mailbox:
 *
 * {register, {pid}} - add the pid for a new cube
 * {left, <message>} - broadcast <message> to the left
 * {right, <message>} - broadcast <message> to the right
 * {update, {pid, value}} - update the UI
 *
 * where <message> is a tuple whose first element is always the
 * sender's pid. Each of NUM_CUBES Erlang processes must
 * send a {register, {pid}} message when started.
 *
 * Simulates the physical world inhabited by a set of Yahtzee Flash cubes.
 * A cube can broadcast a message to its left or right, and based
 * on the physical positions of the cubes in the UI, the message
 * will be delivered to the left or right neighbor if any. The
 * universe also observes the current state of a cube (by calls
 * from the cube to the <code>updateDisplay</code> method) and reports
 * it to the UI.
 */
public class Universe {
    public static final int NUM_CUBES = 5;
    private static UniverseUI ui;

    private static ArrayList<OtpErlangPid> cubes = new ArrayList<OtpErlangPid>();
    private static OtpMbox mbox;

    public static void main(String[] args) {
        Runnable r = new Runnable() {
            @Override
            public void run() {
                bigBang();
            }
        };
        SwingUtilities.invokeLater(r);

        messageLoop();
    }

    /**
     * Creates the universe.
     */
    public static void bigBang() {
        ui = new UniverseUI(NUM_CUBES);
        JFrame frame = new JFrame();
        frame.getContentPane().add(ui);

        // size the frame based on the preferred size of the panel
        frame.pack();

        // make sure it closes when you click the close button on the window
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        // rock and roll...
        frame.setVisible(true);

    }

    public static void messageLoop() {
        // executed in main thread

        System.setProperty("OtpConnection.trace", "4");
        OtpNode node;
        try {
            node = new OtpNode("universe@localhost", "thinmint");
            System.out.println(node);
        } catch (IOException e) {
            System.out.println("Unable to create OtpNode: " + e);
            return;
        }

        mbox = node.createMbox("mailbox");

        while (true) {
            // message format should always be one of:
            //
            // {register, {pid}}
            // {left, <message>}
            // {right, <message>}
            // {update, {pid, value}}
            //
            // where <message> is a tuple whose first element is always the
            // sender's pid

            OtpErlangObject o;
            OtpErlangTuple msg;

            try {
                o = mbox.receive();
                msg = (OtpErlangTuple) o;
                OtpErlangAtom atom = (OtpErlangAtom) msg.elementAt(0);
                final String type = atom.atomValue();
                final OtpErlangTuple tuple = (OtpErlangTuple) msg.elementAt(1);
                final OtpErlangPid pid = (OtpErlangPid) tuple.elementAt(0);

                if ("register".equals(type)) {
                    System.out.println("Registered cube");
                    Runnable r = new Runnable() {
                        @Override
                        public void run() {
                            registerCube(pid);
                        }
                    };
                    SwingUtilities.invokeLater(r);
                } else if ("left".equals(type)) {
                    System.out.println("left");
                    broadcastLeft(tuple);
                } else if ("right".equals(type)) {
                    System.out.println("right");
                    broadcastRight(tuple);
                } else if ("update".equals(type)) {
                    System.out.println("update");
                    OtpErlangLong erlangValue = (OtpErlangLong) tuple.elementAt(1);
                    final int value = erlangValue.intValue();
                    Runnable r = new Runnable() {
                        @Override
                        public void run() {
                            updateDisplay(pid, value);
                        }
                    };
                    SwingUtilities.invokeLater(r);
                } else {
                    System.out.println("Unknown message type: " + type);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

        }
    }

    public static void registerCube(OtpErlangPid pid) {
        if (cubes.size() < NUM_CUBES) {
            cubes.add(pid);
        }
    }

    /**
     * Attempts to send a message to a neighbor on the left, if any.
     *
     * @param msg
     *            message to send
     */
    public static void broadcastLeft(OtpErlangTuple msg) {
        broadcast(msg, true);
    }

    /**
     * Attempts to send a message to a neighbor on the right, if any.
     *
     * @param msg
     *            message to send
     */
    public static void broadcastRight(OtpErlangTuple msg) {
        broadcast(msg, false);
    }

    /**
     * Helper method for broadcasting left or right. This is not executed
     * on event thread.
     *
     * @param msg
     * @param left
     */
    private static void broadcast(OtpErlangTuple msg, boolean left) {
        OtpErlangPid sender = (OtpErlangPid) msg.elementAt(0);

        OtpErlangPid dest = getNeighbor(sender, left);

        if (dest != null) {
            if (mbox != null) {
                mbox.send(dest, msg);
            } else {
                System.out.println("Broadcast error: OtpMbox not initialized.");
            }
        }

    }

    /**
     * Updates the representation of the given component in the UI.
     *
     * @param c
     * @param state
     */
    public static void updateDisplay(OtpErlangPid c, int state) {
        // update UI with value
        int index = getIndex(c);
        if (index < 0)
            return;
        ui.updateDisplay(index, state);
    }

    /**
     * Returns the component with the given index.
     *
     * @param index
     * @return
     */
    private static OtpErlangPid getCube(int index) {
        if (index < 0 || index >= cubes.size()) {
            return null;
        }
        return cubes.get(index);
    }

    /**
     * Returns the index of the given component, or -1 if
     * it is not present.
     *
     * @param c
     * @return
     */
    private static int getIndex(OtpErlangPid c) {
        // this should match using .equals(), which is correctly
        // overridden for OtpErlangPid
        return cubes.indexOf(c);
    }

    /**
     * Returns the neighbor of the given pid, or null if there is no neighbor.
     *
     * @param sender
     * @param left
     * @return
     */
    private static OtpErlangPid getNeighbor(final OtpErlangPid sender, final boolean left) {
        // use an array (which will have size 1) because a blocking queue can't have
        // null elements
        final BlockingQueue<OtpErlangPid[]> queue = new ArrayBlockingQueue<OtpErlangPid[]>(1);

        // do this on event thread, and wait for the result
        Runnable r = new Runnable() {
            @Override
            public void run() {
                // this will contain null if we don't find a neighbor
                OtpErlangPid[] holder = new OtpErlangPid[1];
                int neighbor = -1;
                int index = getIndex(sender);
                if (index >= 0) {
                    neighbor = ui.getNeighbor(index, left);
                    OtpErlangPid pid = getCube(neighbor);
                    holder[0] = pid;
                }
                queue.add(holder);
            }
        };
        SwingUtilities.invokeLater(r);

        try {
            // wait for UI to set result
            OtpErlangPid[] holder = queue.take();
            return holder[0];
        } catch (InterruptedException e) {
            return null;
        }
    }

}
