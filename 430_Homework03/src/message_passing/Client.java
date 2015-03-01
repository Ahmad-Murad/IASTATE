package message_passing;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public class Client extends Component {

    private Component input;
    private Component proxy = new Proxy();
    private Component timeout = new Timeout();
    private static final long TIMEOUT = 5500L; // 5.5 seconds

    /**
     * Key: the key value being requested from the database
     * Value: the number of requests which have been make for the given key
     */
    private Map<Integer, Integer> pendingRequests = new HashMap<>();
    /**
     * Key: correlation id
     * Value: the id value requested from the DB
     */
    private Map<Integer, Integer> correlationIDtoKey = new HashMap<>();
    private Map<Integer, Record> cache = new HashMap<Integer, Record>();
    private LinkedBlockingQueue<IMessage> messages = new LinkedBlockingQueue<>();

    private boolean done = false;

    public static void main(String[] args) {
        new Client().start();
    }

    /**
     * Main client loop.
     */
    @Override
    public void start() {
        input = new Input(this);
        input.start();
        proxy.start();
        timeout.start();
        IMessage cur = null;
        do {
            try {
                cur = messages.poll(1L, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
            }
            if (cur != null)
                cur.dispatch(this);
        } while (!done);
        // input will stop on its own, no need to send it StopMessage
        proxy.send(new StopMessage(this));
        timeout.send(new StopMessage(this));
    }

    /**
     * Parses the string entered by user and takes appropriate action.
     *
     * @param s
     */
    private void parseResponse(String s) {
        s = s.trim();
        if (isNumeric(s)) {
            int key = Integer.parseInt(s);
            doLookupAndPrint(key);
        } else {
            char ch = s.charAt(0);
            if (ch == 'd') {
                display();
            } else if (ch == 'q') {
                done = true;
            } else {
                System.out.println("Please enter 'd', 'q', or an id number");
            }
        }
    }

    /**
     * Returns the value for the given key, retrieving it from the slow database
     * if not present in the local list.
     *
     * @param key
     * @return
     */
    private void doLookupAndPrint(int key) {
        String value = getLocalValue(key);
        if (value == null) {
            // Acquire a lock on the HashMap so a series of operations can be done atomically
            synchronized (pendingRequests) {
                if (!pendingRequests.containsKey(key)) {
                    RequestMessage request = new RequestMessage(this, key);
                    TimeoutMessage timeoutMsg = new TimeoutMessage(request.getId(), this, TIMEOUT);
                    correlationIDtoKey.put(request.getId(), key);
                    // If this key hasn't been requested before, add it to the pending requests
                    pendingRequests.put(key, 1);
                    proxy.send(request);
                    timeout.send(timeoutMsg);
                } else {
                    // If this key has been requested already (but not returned and cached yet)
                    // then increment the number of pending requests for this key.
                    pendingRequests.put(key, pendingRequests.get(key) + 1);
                }
            }
        } else {
            printResponse(key, value);
        }
    }

    /**
     * Returns the value for given key, or null if not present in the list.
     *
     * @param key
     * @return
     */
    private String getLocalValue(int key) {
        synchronized (cache) {
            Record r = cache.get(key);
            return r == null ? null : r.value();
        }
    }

    /**
     * Returns true if the given string represents a positive integer.
     *
     * @param s
     * @return
     */
    private boolean isNumeric(String s) {
        for (int i = 0; i < s.length(); ++i) {
            if (!Character.isDigit(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    /**
     * Displays all key/value pairs in local list.
     */
    private void display() {
        synchronized (cache) {
            for (Integer i : cache.keySet()) {
                Record r = cache.get(i);
                System.out.println(r.key() + " " + r.value());
            }
        }
    }

    private void printResponse(int key, String value) {
        System.out.println("Value for id " + key + ": " + value);
    }

    /**
     * Key/value pair.
     */
    private static class Record implements Comparable<Record> {
        private final int key;
        private final String value;

        public Record(int key, String value) {
            this.key = key;
            this.value = value;
        }

        public int key() {
            return key;
        }

        public String value() {
            return value;
        }

        @Override
        public int compareTo(Record rhs) {
            return this.key - rhs.key;
        }
    }

    @Override
    public void send(IMessage message) {
        messages.add(message);
    }

    @Override
    public void handle(TextMessage msg) {
        parseResponse(msg.getText());
    }

    @Override
    public void handle(ResultMessage msg) {
        Integer key = correlationIDtoKey.remove(msg.getCorrelationId());
        if (key == null)
            return; // this request has already been completed

        String result = msg.getResult();
        if ("TIMED_OUT".equalsIgnoreCase(result)) {
            System.out.println("Request " + msg.getCorrelationId() + " has timed out after " + TIMEOUT + "ms.  Any late responses will be ignored for this request.");
        } else {
            synchronized (cache) {
                if (getLocalValue(key) == null) {
                    cache.put(key, new Record(key, result));
                }
            }
        }

        // Now that we have finished getting a value from the database, print out the results
        // however many times the key has been requested since the original request.
        synchronized (pendingRequests) {
            if (!"TIMED_OUT".equalsIgnoreCase(result))
                for (int numRequests = pendingRequests.get(key); numRequests > 0; numRequests--) {
                    printResponse(key, result);
                }
            pendingRequests.remove(key);
        }
    }
}
