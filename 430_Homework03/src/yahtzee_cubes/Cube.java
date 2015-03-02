package yahtzee_cubes;

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
//    private LinkedBlockingQueue<IMessage> messages = new LinkedBlockingQueue<>();
    private IMessage[] recentMessages = new IMessage[Universe.NUM_CUBES];
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
        synchronized (recentMessages) {
            recentMessages[message.getCorrelationId()] = message;
        }
//        messages.add(message);
    }

    @Override
    public void handle(LocationMessage msg)
    {
        int max = 0;
//        recentMessages[msg.getCorrelationId()] = msg;
        synchronized (recentMessages) {
            for (int i = 0; i < recentMessages.length; i++) {
                LocationMessage lm = (LocationMessage) recentMessages[i];
                if (lm != null && lm.isExpired())
                    recentMessages[i] = null;
                else if (lm != null && max < lm.getCubesSeen() + 1)
                    max = lm.getCubesSeen() + 1;
            }
        }
        if (max != position) {
            position = max;
            Universe.updateDisplay(this, position);
        }
        if (msg.getCubesSeen() != 0)
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
                    boolean alone = true;
                    for (int i = 0; i < recentMessages.length; i++) {
                        if (recentMessages[i] != null) {
                            recentMessages[i].dispatch(self);
                            alone = false;
                        }
                    }
                    if (alone) {
                        self.position = 0;
                        Universe.updateDisplay(self, 0);
                    }
                    sendHeartbeat();
                    idleFor(POLL_FREQ);
                } while (!done);
            }
        }, this.toString()).start();
    }

    @Override
    public String toString() {
        return "Cube-" + cubeID;
    }

    public void sendHeartbeat() {
        Universe.broadcastRight(new LocationMessage(this.cubeID, this, 1, TIMEOUT));
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