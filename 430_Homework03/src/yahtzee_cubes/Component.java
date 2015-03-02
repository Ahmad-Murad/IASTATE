package yahtzee_cubes;

/**
 * Base component type for an actor-style message-passing
 * programming model.
 */
public abstract class Component
{
    /**
     * Sink for log messages.
     */
    protected Component logger;

    protected void log(String logString)
    {
        logString = this.getClass() + ": " + logString;
        TextMessage m = new TextMessage(this, logString);
        logger.send(m);
    }

    /**
     * Sends a message to this component. The implementation
     * of this method could vary between components but in all
     * case it should accept messages without blocking.
     *
     * @param message
     *            the message to send
     */
    public abstract void send(IMessage message);

    /**
     * Signals to this component that it may begin processing messages.
     */
    public abstract void start();

    /**
     * Default message handling method.
     *
     * @param msg
     */
    public void handle(IMessage msg)
    {
        log("Unhandled message: " + msg.toString());
    }

    // Overload the handle() method for each concrete message type so that
    // it calls the default handle() implementation above.  These methods
    // are overridden by components that expect to handle particular
    // message types.

    public void handle(TextMessage msg)
    {
        handle((IMessage) msg);
    }

    public void handle(LocationMessage msg)
    {
        handle((IMessage) msg);
    }

}