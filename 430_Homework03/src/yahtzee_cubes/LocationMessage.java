package yahtzee_cubes;

/**
 * A concrete message which tracks two things:
 * <li>The number of cubes known to be left of the sender
 * <li>Some expiration time which indicates the maximum time which this message is valid.
 * 
 * @author Andrew
 */
public class LocationMessage extends AbstractMessage
{
    private final int cubesSeen;
    private final long expiresAt;

    public LocationMessage(int correlationId, Component sender, int seen, long timeToLive)
    {
        super(correlationId, sender);
        this.cubesSeen = seen;
        expiresAt = System.currentTimeMillis() + timeToLive;
    }

    public LocationMessage(Component sender, int seen, long timeToLive)
    {
        super(sender);
        this.cubesSeen = seen;
        expiresAt = System.currentTimeMillis() + timeToLive;
    }

    @Override
    public void dispatch(Component receiver)
    {
        receiver.handle(this);
    }

    public int getCubesSeen() {
        return cubesSeen;
    }

    public boolean isExpired() {
        return System.currentTimeMillis() > expiresAt;
    }

    public boolean isRightbound() {
        return cubesSeen > 0;
    }

    @Override
    public String toString() {
        return "(id=" + this.id + "  corr=" + this.correlationId + "  count=" + cubesSeen + "  sender=" + this.getSender() + ")";
    }

    /**
     * Construct a new LocationMessage from this one.
     * The correlationID is cloned from the original, cubesSeen is incremented by one,
     * and the expiresAt time is the same as the original as well.
     */
    public LocationMessage forward(Component sender) {
        long timeToLive = this.expiresAt - System.currentTimeMillis();
        return new LocationMessage(this.getCorrelationId(), sender, this.cubesSeen + 1, timeToLive);
    }
}
