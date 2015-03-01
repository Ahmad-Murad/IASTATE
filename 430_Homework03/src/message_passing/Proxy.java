package message_passing;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * @author Andrew
 */
public class Proxy extends Component {

    private static final String HOST = "localhost";
    private static final int PORT = 2222;
    private final int NUM_WORKERS = 5;
    private volatile boolean done = false;
    private LinkedBlockingQueue<IMessage> messages = new LinkedBlockingQueue<>();
    private final ExecutorService workers = Executors.newFixedThreadPool(NUM_WORKERS);

    @Override
    public void start() {
        final Proxy self = this;
        new Thread(new Runnable() {
            @Override
            public void run() {
                for (int i = 0; i < NUM_WORKERS; i++)
                    workers.execute(new QueuePollingWorker(self));
                workers.shutdown(); // signal shutdown once all workers complete
            }
        }, "Proxy Component Thread").start();
    }

    @Override
    public void send(IMessage message) {
        messages.add(message);
    }

    @Override
    public void handle(StopMessage msg)
    {
        this.done = true;
    }

    @Override
    public void handle(RequestMessage msg) {
        String value = getValueFromDB(msg.getKey());
        msg.getSender().send(new ResultMessage(msg.getId(), this, value));
    }

    private String getValueFromDB(int key) {
        Socket s = null;
        try {
            // open a connection to the server
            s = new Socket(HOST, PORT);

            // for line-oriented output we use a PrintWriter
            PrintWriter pw = new PrintWriter(s.getOutputStream());
            pw.println("" + key);
            pw.flush(); // don't forget to flush...

            // read response, which we expect to be line-oriented text
            Scanner scanner = new Scanner(s.getInputStream());
            String result = scanner.nextLine();
            scanner.close();
            return result;
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            // be sure streams are closed
            try {
                s.close();
            } catch (IOException ignore) {
            }
        }
    }

    private class QueuePollingWorker implements Runnable {

        final Component proxy;

        public QueuePollingWorker(Proxy self) {
            this.proxy = self;
        }

        @Override
        public void run() {
            IMessage cur = null;
            do {
                try {
                    cur = messages.poll(1L, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                }
                if (cur != null) {
                    cur.dispatch(proxy);
                }
            } while (!done);
        }
    }
}
