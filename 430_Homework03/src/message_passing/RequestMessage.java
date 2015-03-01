package message_passing;

public class RequestMessage extends AbstractMessage
{
    protected final int key;

    public RequestMessage(Component sender, int key)
    {
        super(sender);
        this.key = key;
    }

    public int getKey()
    {
        return key;
    }

    @Override
    public void dispatch(Component receiver)
    {
        receiver.handle(this);
    }
}