package yahtzee_cubes;

/**
 * @author Andrew
 */
public class LocationMessage extends AbstractMessage
{
    private final int cubesSeen;

    public LocationMessage(int correlationId, Component sender, int seen)
    {
        super(correlationId, sender);
        this.cubesSeen = seen;
    }

    public LocationMessage(Component sender, int seen)
    {
        super(sender);
        this.cubesSeen = seen;
    }

    @Override
    public void dispatch(Component receiver)
    {
        System.out.println(toString() + " is dispatching to " + receiver);
        receiver.handle(this);
    }

    public int getCubesSeen() {
        return cubesSeen;
    }

    @Override
    public String toString() {
        return "(id=" + this.id + "  corr=" + this.correlationId + "  count=" + cubesSeen + "  sender=" + this.getSender() + ")";
    }

    public LocationMessage forward(Component sender) {
        return new LocationMessage(this.getCorrelationId(), sender, this.cubesSeen + 1);
    }
}
