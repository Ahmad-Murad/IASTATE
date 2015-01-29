package hw1;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.Scanner;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Client changes:<br>
 * Whenever new input is received, first it gets parsed and checked against the cache.
 * If the item is not in the cache and thus requires DB access, then a new thread is created
 * which will finish asynchronously so the client UI is free to accept additional input.
 * Note that the DBTask thread prints its own response, so the UI thread does not have to
 * interact with the DBTask thread at all once it is started. <p>
 * SimpleServer changes:<br>
 * Changed the server so the only thing the main thread does is accept new sockets and spin
 * off new threads. The worker threads created execute database queries and return results
 * to the client whenever the query completes. When the worker thread completes a query
 * it closes it's own socket and then terminates. <p>
 * Optional section:<br>
 * Added a ConcurrentHashMap for tracking current key requests to database (key portion of map)
 * and how many requests had been made for the corresponding key (value portion of map). When
 * a new request is made, we check the map to see if there are already pending requests for that
 * key. If there are pending requests, increment requests by 1, if not put (key, 1) into the map.
 *
 * @author Andrew
 */
public class Client {
    public static final String HOST = "localhost";
    public static final int PORT = 2222;

    /**
     * Local cache of key/value pairs we've already looked up.
     */
    private ConcurrentHashMap<Integer, Record> cache;
    /**
     * Map which contains a list of the current pending key requests.
     * Key: the key value being requested from the database
     * Value: the number of requests which have been make for the given key
     */
    private ConcurrentHashMap<Integer, Integer> pendingRequests;

    /**
     * Scanner for console input.
     */
    private Scanner scanner;

    /**
     * Indicates whether the client should be shut down.
     */
    private boolean done = false;

    /**
     * Entry point.
     *
     * @param args
     */
    public static void main(String[] args) {
        new Client().go();
    }

    public Client() {
        cache = new ConcurrentHashMap<Integer, Record>();
        pendingRequests = new ConcurrentHashMap<Integer, Integer>();
        scanner = new Scanner(System.in);
    }

    /**
     * Main client loop.
     */
    public void go() {
        System.out.println("\nEnter id number to look up, 'd' to display list, 'q' to quit");
        while (!done) {
            String response = getResponse();
            parseResponse(response);
        }
    }

    /**
     * Prints a menu and returns the text entered by user.
     *
     * @return
     */
    private String getResponse() {
        System.out.print("Your choice:\n");
        return scanner.nextLine();
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
                    // If this key hasn't been requested before, add it to the pending requests
                    pendingRequests.put(key, 1);
                    new DBTask(key);
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
        for (Record r : cache.values()) {
            if (r.key() == key) {
                return r.value();
            }
        }
        return null;
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
        for (Integer i : cache.keySet()) {
            Record r = cache.get(i);
            System.out.println(r.key() + " " + r.value());
        }
    }

    private class DBTask implements Runnable {
        private final Thread t;
        private final int key;

        public DBTask(int k) {
            key = k;
            t = new Thread(this);
            t.start();
        }

        @Override
        public void run() {
            getValueFromDB(key);
            String value = getLocalValue(key);

            // Now that we have finished getting a value from the database, print out the results
            // however many times the key has been requested since the original request.
            synchronized (pendingRequests) {
                for (int numRequests = pendingRequests.get(key); numRequests > 0; numRequests--) {
                    printResponse(key, value);
                }
                pendingRequests.remove(key);
            }
        }

        /**
         * Look up given key in the slow database and add it to the local list.
         *
         * @param key
         */
        private void getValueFromDB(int key) {
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
                if (getLocalValue(key) == null) {
                    cache.put(key, new Record(key, result));
                }
                scanner.close();
            } catch (IOException e) {
                System.out.println(e);
            } finally {
                // be sure streams are closed
                try {
                    s.close();
                } catch (IOException ignore) {
                }
            }
        }
    }

    private synchronized void printResponse(int key, String value) {
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
}
