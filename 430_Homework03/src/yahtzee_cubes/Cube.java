package yahtzee_cubes;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Component representing a Yahtzee Flash cube.
 */
public class Cube extends Component
{
    private static final long POLL_FREQ = 50;
    private static final long TIMEOUT = 500;
    private static int cubeIDcount = 0;
    private final int cubeID;
    private TimerComponent timer;
    private boolean done = false;
    private LinkedBlockingQueue<IMessage> messages = new LinkedBlockingQueue<>();
    private LocationMessage[] recentMessages = new LocationMessage[Universe.NUM_CUBES];
    private int position = 0;

    public Cube(TimerComponent timer)
    {
        this.cubeID = cubeIDcount++;
        this.timer = timer;
        for (int i = 0; i < Universe.NUM_CUBES; i++)
            recentMessages[i] = null;
    }

    @Override
    public void send(IMessage message) {
        messages.add(message);
    }

    @Override
    public void handle(LocationMessage msg)
    {
        if (msg.isExpired())
            return;

        int max = 0;
        synchronized (recentMessages) {
            recentMessages[msg.getCorrelationId()] = msg;
            for (int i = 0; i < recentMessages.length; i++) {
                if (recentMessages[i] != null && recentMessages[i].isExpired())
                    recentMessages[i] = null;
                if (recentMessages[i] != null && max < recentMessages[i].getCubesSeen() + 1)
                    max = recentMessages[i].getCubesSeen() + 1;
            }
        }
        if (max != position) {
            position = max;
            Universe.updateDisplay(this, position);
        }
        if (msg.getCubesSeen() != 0)
            Universe.broadcastRight(msg.forward(this));
        idleFor(POLL_FREQ);
    }

    @Override
    public void start()
    {
        final Cube self = this;
        new Thread(new Runnable() {
            @Override
            public void run() {
                do {
                    LocationMessage msg = new LocationMessage(self.cubeID, self, 1, TIMEOUT);
                    Universe.broadcastRight(msg);
                    try {
                        IMessage cur = self.messages.poll(POLL_FREQ, TimeUnit.MILLISECONDS);
                        if (cur != null) {
                            cur.dispatch(self);
                        } else if (self.position != 0) {
                            // nothing on the queue, must be alone
                            self.position = 0;
                            Universe.updateDisplay(self, self.position);
                        }
                    } catch (InterruptedException e) {
                    }
                    sendHeartbeat();
                } while (!done);
            }
        }, this.toString()).start();
    }

    @Override
    public String toString() {
        return "Cube-" + cubeID;
    }

    public void sendHeartbeat() {
//        messages.clear();
        synchronized (recentMessages) {
            for (int i = 0; i < recentMessages.length; i++)
                if (recentMessages[i] != null && recentMessages[i].isExpired())
                    recentMessages[i] = null;
        }
        Universe.broadcastLeft(new LocationMessage(this.cubeID, this, 0, TIMEOUT));
    }

    /**
     * sleep for some amount of ms, in 50ms increments
     *
     * @param ms
     */
    public void idleFor(long ms) {
        long end = System.currentTimeMillis() + ms;
        while (System.currentTimeMillis() < end) {
            try {
                Thread.sleep(50);
            } catch (InterruptedException e) {
            }
        }
    }
}