package yahtzee_cubes;

/**
 * @author Andrew
 */
public class TimeoutMessage extends AbstractMessage
{
    private final long timeoutAt;
    private final boolean isRight;

    public TimeoutMessage(int correlationId, Component sender, long timeToLivems, boolean right)
    {
        super(correlationId, sender);
        if (timeToLivems < 0)
            throw new RuntimeException("Cannot specify a negative timeout");
        this.timeoutAt = System.currentTimeMillis() + timeToLivems;
        this.isRight = right;
    }

    @Override
    public void dispatch(Component receiver)
    {
        receiver.handle(this);
    }

    public boolean hasTimedOut() {
        return timeoutAt < System.currentTimeMillis();
    }

    public boolean isRight() {
        return isRight;
    }
}
