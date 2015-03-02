package yahtzee_cubes;

import java.util.concurrent.LinkedBlockingQueue;

/**
 * Component for generating periodic messages and
 * timeout messages.
 */
public class TimerComponent extends Component
{
    private LinkedBlockingQueue<IMessage> messages = new LinkedBlockingQueue<>();
    private volatile boolean done = false;

    @Override
    public void start() {
        final Component self = this;
        new Thread(new Runnable() {
            @Override
            public void run() {
                IMessage cur = null;
                do {
                    cur = messages.poll();
                    if (cur != null)
                        cur.dispatch(self);
                } while (!done);
            }
        }, "Timeout Component Thread").start();
    }

    @Override
    public void send(IMessage message) {
        messages.add(message);
    }

    @Override
    public void handle(TimeoutMessage msg)
    {
        if (msg.hasTimedOut()) {
            // notify the sender that the timeout has been reached
            msg.getSender().send(msg);
        }
        else {
            // if the message hasn't timed out yet, add it to the back of the queue
            messages.add(msg);
        }
    }
}