package message_passing;

public class ResultMessage extends AbstractMessage
{
    protected final String result;

    public ResultMessage(int correlationId, Component sender, String result)
    {
        super(correlationId, sender);
        this.result = result;
    }

    public String getResult()
    {
        return result;
    }

    @Override
    public void dispatch(Component receiver)
    {
        receiver.handle(this);
    }
}