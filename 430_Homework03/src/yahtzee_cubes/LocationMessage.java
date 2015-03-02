package yahtzee_cubes;

/**
 * @author Andrew
 */
public class LocationMessage extends AbstractMessage
{
    private final int cubesSeen;
    private final long expiresAt;
    private final long timeToLive;

    public LocationMessage(int correlationId, Component sender, int seen, long timeToLive)
    {
        super(correlationId, sender);
        this.cubesSeen = seen;
        this.timeToLive = timeToLive;
        expiresAt = System.currentTimeMillis() + timeToLive;
    }

    public LocationMessage(Component sender, int seen, long timeToLive)
    {
        super(sender);
        this.cubesSeen = seen;
        this.timeToLive = timeToLive;
        expiresAt = System.currentTimeMillis() + timeToLive;
    }

    @Override
    public void dispatch(Component receiver)
    {
//        System.out.println(toString() + " is dispatching to " + receiver);
        receiver.handle(this);
    }

    public int getCubesSeen() {
        return cubesSeen;
    }

    public boolean isExpired() {
//        if (expiresAt == Long.MAX_VALUE)
//            throw new RuntimeException("isExpired was called too early.");
        return System.currentTimeMillis() > expiresAt;
    }

    @Override
    public String toString() {
        return "(id=" + this.id + "  corr=" + this.correlationId + "  count=" + cubesSeen + "  sender=" + this.getSender() + ")";
    }

    public LocationMessage forward(Component sender) {
        return new LocationMessage(this.getCorrelationId(), sender, this.cubesSeen + 1, this.timeToLive);
    }
}
