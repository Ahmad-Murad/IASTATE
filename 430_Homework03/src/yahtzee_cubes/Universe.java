package yahtzee_cubes;

import java.util.concurrent.CountDownLatch;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

/**
 * Simulates the physical world inhabited by a set of Yahtzee Flash cubes.
 * A cube can broadcast a message to its left or right, and based
 * on the physical positions of the cubes in the UI, the message
 * will be delivered to the left or right neighbor if any. The
 * universe also observes the current state of a cube and reports
 * it to the UI.
 * <p>
 * The Component objects representing the cubes should invoke
 * updateDisplay() whenever their internal state changes. In this
 * simplified implementation, the state is just a number 0 through 5,
 * where 0 represents "unknown" and is the value reported by any cube
 * that has no neighbors.
 */
public class Universe
{
    public static final int NUM_CUBES = 5;
    public static final CountDownLatch startLatch = new CountDownLatch(NUM_CUBES);

    // Statically initialized fields are guaranteed to be correctly visible to
    // all threads
    private static final Component[] cubes;
    private static final TimerComponent timer;

    // The methods of the UI that are called from this class are
    // thread-safe, but we need to make sure the reference is visible
    private static volatile UniverseUI ui;

    static
    {
        // TODO - construct the cubes according to your implementation,
        // your setup may be different from what's shown below
        cubes = new Component[NUM_CUBES];
        timer = new TimerComponent();
        for (int i = 0; i < NUM_CUBES; ++i)
        {
            cubes[i] = new Cube(timer);
        }
    }

    public static void main(String[] args) throws Exception
    {
        Runnable r = new Runnable()
        {
            @Override
            public void run()
            {
                bigBang();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    /**
     * Creates the universe.
     */
    public static void bigBang()
    {
        // user interface (this is how God controls the universe)
        ui = new UniverseUI(NUM_CUBES);
        JFrame frame = new JFrame();
        frame.getContentPane().add(ui);

        // size the frame based on the preferred size of the panel
        frame.pack();

        // make sure it closes when you click the close button on the window
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        // and on the 6th day...
        frame.setVisible(true);

        // start up all the components
        for (int i = 0; i < NUM_CUBES; ++i)
        {
            cubes[i].start();
        }
        timer.start();
    }

    /**
     * Attempts to send a message to a neighbor on the left, if any.
     *
     * @param msg
     *            message to send
     */
    public static void broadcastLeft(IMessage msg)
    {
        broadcast(msg, true);
    }

    /**
     * Attempts to send a message to a neighbor on the right, if any.
     *
     * @param msg
     *            message to send
     */
    public static synchronized void broadcastRight(IMessage msg)
    {
        broadcast(msg, false);
    }

    /**
     * Helper method for broadcasting left or right.
     *
     * @param msg
     * @param left
     */
    private static void broadcast(IMessage msg, boolean left)
    {
        int index = getIndex(msg.getSender());
        if (index < 0)
            return;
        int neighbor = ui.getNeighbor(index, left);
        if (neighbor >= 0)
        {
            getCube(neighbor).send(msg);
        }
    }

    /**
     * Updates the representation of the given component in the UI.
     *
     * @param c
     * @param state
     */
    public static void updateDisplay(Component c, int state)
    {
        System.out.println(c.toString() + " changing to: " + state);
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
    private static Component getCube(int index)
    {
        return cubes[index];
    }

    /**
     * Returns the index of the given component, or -1 if
     * it is not present.
     *
     * @param c
     * @return
     */
    private static int getIndex(Component c)
    {
        for (int i = 0; i < cubes.length; ++i)
        {
            if (c == cubes[i])
            {
                return i;
            }
        }
        return -1;
    }
}