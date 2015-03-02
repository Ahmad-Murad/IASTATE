package yahtzee_cubes;

/**
 * Component representing a Yahtzee Flash cube.
 *
 * @author Andrew
 */
public class Cube extends Component
{
    private static final long POLL_FREQ = 50;
    private static final long TIMEOUT = 500;
    private static int cubeIDcount = 0;
    private final int cubeID;
    /**
     * An array of IMessages which stores the most recent message received from each cube.
     */
    private IMessage[] recentMessages = new IMessage[Universe.NUM_CUBES];
    private int position = 0;

    public Cube()
    {
        this.cubeID = cubeIDcount++;
        for (int i = 0; i < Universe.NUM_CUBES; i++)
            recentMessages[i] = null;
    }

    @Override
    public void send(IMessage message) {
        // put the message into the corresponding array index of whoever sent the message
        // this is determined by the correlationID of the message
        synchronized (recentMessages) {
            recentMessages[message.getCorrelationId()] = message;
        }
    }

    @Override
    public void handle(LocationMessage msg)
    {
        int maxPos = 0;
        synchronized (recentMessages) {
            // look at the most recent message from each cube
            for (int i = 0; i < recentMessages.length; i++) {
                LocationMessage locMsg = (LocationMessage) recentMessages[i];
                if (locMsg != null && locMsg.isExpired())
                    recentMessages[i] = null; // remove expired messages
                else if (locMsg != null && maxPos < locMsg.getCubesSeen() + 1)
                    maxPos = locMsg.getCubesSeen() + 1;
            }
        }

        // if the cube's position has changed, update the display
        if (maxPos != position) {
            position = maxPos;
            Universe.updateDisplay(this, position);
        }

        // don't forward leftbound messages to the right
        if (msg.isRightbound())
            Universe.broadcastRight(msg.forward(this));
    }

    @Override
    public void start()
    {
        final Cube self = this;
        new Thread(new Runnable() {
            @Override
            public void run() {
                // Iterate over and dispatch all of the most recent messages
                // received from each cube every POLL_FREQ ms.
                do {
                    boolean alone = true;
                    // it is ok to NOT synchronize on recentMessages here because
                    // we poll the array every POLL_FREQ milliseconds
                    for (int i = 0; i < recentMessages.length; i++) {
                        IMessage cur = recentMessages[i];
                        if (cur != null) {
                            cur.dispatch(self);
                            alone = false;
                        }
                    }
                    if (alone) {
                        self.position = 0;
                        Universe.updateDisplay(self, 0); // set to '?'
                    }
                    sendHeartbeat();
                    idleFor(POLL_FREQ);
                } while (true);
            }
        }, this.toString()).start();
    }

    @Override
    public String toString() {
        return "Cube-" + cubeID;
    }

    /**
     * Sends a LocationMesage to the right and to the left
     */
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