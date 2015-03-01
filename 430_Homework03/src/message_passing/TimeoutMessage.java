package message_passing;

/**
 * @author Andrew
 */
public class TimeoutMessage extends AbstractMessage
{
    private final long timeoutAt;

    public TimeoutMessage(int correlationId, Component sender, long timeToLivems)
    {
        super(correlationId, sender);
        if (timeToLivems < 0)
            throw new RuntimeException("Cannot specify a negative timeout");
        this.timeoutAt = System.currentTimeMillis() + timeToLivems;
    }

    @Override
    public void dispatch(Component receiver)
    {
        receiver.handle(this);
    }

    public boolean hasTimedOut() {
        return timeoutAt < System.currentTimeMillis();
    }
}
