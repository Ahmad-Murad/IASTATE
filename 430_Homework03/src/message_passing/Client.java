package message_passing;

import java.util.concurrent.ConcurrentLinkedQueue;

public class Client {

    private ConcurrentLinkedQueue<IMessage> pendingRequests = new ConcurrentLinkedQueue<>();

    public static void main(String[] args) {
        new Client().go();
    }

    public void go() {
        new InputComponent(this).start();
    }

    public synchronized void addMessage(IMessage toAdd) {
        pendingRequests.add(toAdd);
    }

//    private void doLookupAndPrint(int key) {
//        String value = getLocalValue(key);
//        if (value == null) {
//            // Acquire a lock on the HashMap so a series of operations can be done atomically
//            synchronized (pendingRequests) {
//                if (!pendingRequests.containsKey(key)) {
//                    // If this key hasn't been requested before, add it to the pending requests
//                    pendingRequests.put(key, 1);
//                    new DBTask(key);
//                } else {
//                    // If this key has been requested already (but not returned and cached yet)
//                    // then increment the number of pending requests for this key.
//                    pendingRequests.put(key, pendingRequests.get(key) + 1);
//                }
//            }
//        } else {
//            printResponse(key, value);
//        }
//    }

//    private class DBTask implements Runnable {
//        private final Thread t;
//        private final int key;
//
//        public DBTask(int k) {
//            key = k;
//            t = new Thread(this);
//            t.start();
//        }
//
//        @Override
//        public void run() {
//            getValueFromDB(key);
//            String value = getLocalValue(key);
//
//            // Now that we have finished getting a value from the database, print out the results
//            // however many times the key has been requested since the original request.
//            synchronized (pendingRequests) {
//                for (int numRequests = pendingRequests.get(key); numRequests > 0; numRequests--) {
//                    printResponse(key, value);
//                }
//                pendingRequests.remove(key);
//            }
//        }
//    }
}
