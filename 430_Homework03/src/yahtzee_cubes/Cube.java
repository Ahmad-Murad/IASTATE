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
    private int[] counts = new int[Universe.NUM_CUBES];
    private int position = 1;

    public Cube(TimerComponent timer)
    {
        this.cubeID = cubeIDcount++;
        this.timer = timer;
        for (int i = 0; i < Universe.NUM_CUBES; i++)
            counts[i] = 0;
    }

    @Override
    public void send(IMessage message) {
        messages.add(message);
    }

    @Override
    public void handle(LocationMessage msg)
    {
        counts[msg.getCorrelationId()] = msg.getCubesSeen();
        int max = 0;
        for (int i = 0; i < Universe.NUM_CUBES; i++)
            if (max < counts[i])
                max = counts[i];
        if (max != position) {
            position = max;
            Universe.updateDisplay(this, position);
        }
        Universe.broadcastRight(msg.forward(this));
    }

    @Override
    public void start()
    {
        final Cube self = this;
        new Thread(new Runnable() {
            @Override
            public void run() {
                do {
                    LocationMessage msg = new LocationMessage(self.cubeID, self, 1);
                    Universe.broadcastRight(msg);
                    try {
                        IMessage cur = self.messages.poll(POLL_FREQ, TimeUnit.MILLISECONDS);
                        if (cur != null)
                            cur.dispatch(self);
                        else {
                            // nothing on the queue, must be alone
                            Universe.updateDisplay(self, 0);
                        }
                    } catch (InterruptedException e) {
                    }
                    idleFor(POLL_FREQ);
                } while (!done);
            }
        }, "Cube-" + this.cubeID).start();
    }

    @Override
    public String toString() {
        return "Cube-" + cubeID;
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