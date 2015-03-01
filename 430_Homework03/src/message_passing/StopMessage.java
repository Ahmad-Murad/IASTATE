/**
 *
 */
package message_passing;

/**
 * @author Andrew
 *
 */
public class StopMessage extends AbstractMessage
{
    public StopMessage(Component sender)
    {
        super(sender);
    }

    public StopMessage(int correlationId, Component sender)
    {
        super(correlationId, sender);
    }

    @Override
    public void dispatch(Component receiver)
    {
        receiver.handle(this);
    }
}
