package client;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

import Ice.LocalException;
import Ice.UserException;
import dbexample.Callback_Database_retrieve;
import dbexample.DatabasePrx;
import dbexample.NoSuchEntryException;

/**
 * Client that uses a callback object to make an async call on the database,
 * and uses synchronization for that thread to safely update
 * the cache.
 */
public class ClientWithCallback {

    /**
     * Reference to database proxy.
     */
    private DatabasePrx db;

    /**
     * Local cache of key/value pairs we've already looked up.
     */
    private ArrayList<Record> cache;

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
        int status = 0;
        Ice.Communicator ic = null;
        try {
//            ic = Ice.Util.initialize(args);
            ic = Ice.Util.initialize(new String[] { "--Ice.ThreadPool.Client.Size=10" });
//            Ice.ObjectPrx base = ic.stringToProxy("SimplePrinter:default -p 10000");
            Ice.ObjectPrx base = ic.stringToProxy("Simpledbexample:default -p 10000");
//            Demo.PrinterPrx printer = Demo.PrinterPrxHelper.checkedCast(base);
            dbexample.DatabasePrx db = dbexample.DatabasePrxHelper.checkedCast(base);
            if (db == null)
                throw new Error("Invalid proxy");

//            db.printString("Hello World!");
            new ClientWithCallback(db).go();
        } catch (Ice.LocalException e) {
            e.printStackTrace();
            status = 1;
        } catch (Exception e) {
            System.err.println(e.getMessage());
            status = 1;
        }
        if (ic != null) {
            // Clean up
            try {
                ic.destroy();
            } catch (Exception e) {
                System.err.println(e.getMessage());
                status = 1;
            }
        }
        System.exit(status);
    }

    public ClientWithCallback(DatabasePrx database) {
        cache = new ArrayList<Record>();
        scanner = new Scanner(System.in);
        this.db = database;
//        this.db = new DatabasePrxHelper();
    }

    /**
     * Main client loop.
     */
    public void go() {
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
        System.out.println();
        System.out.println("Enter id number to look up, 'd' to display list, 'q' to quit");
        System.out.print("Your choice: ");
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
            doLookup(key);
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
     * Looks up the value for the given key, retrieving it from the
     * slow database if not present in the local list.
     *
     * @param key
     * @throws NoSuchEntryException
     */
    private void doLookup(int key) {
        String value = getLocalValue(key);
        if (value == null) {
            try {
                getValueFromDB(key);
            } catch (NoSuchEntryException e) {
                System.out.println("Entry not found in DB");
            }
        } else {
            System.out.println("Value for id " + key + ": " + value);
        }
    }

    /**
     * Look up given key in the slow database and add it to the local list.
     *
     * @param key
     * @throws NoSuchEntryException
     */
    private void getValueFromDB(int key) throws NoSuchEntryException {
        db.begin_retrieve(key, new Callback(key));
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
    private synchronized void display() {
        for (int i = 0; i < cache.size(); ++i) {
            Record r = cache.get(i);
            System.out.println(r.key() + " " + r.value());
        }
    }

    /**
     * Returns the value for given key, or null if not present in the list.
     *
     * @param key
     * @return
     */
    private synchronized String getLocalValue(int key) {
        for (Record r : cache) {
            if (r.key() == key) {
                return r.value();
            }
        }
        return null;
    }

    private class Callback extends Callback_Database_retrieve {
        private int key;

        public Callback(int key) {
            this.key = key;
        }

        @Override
        public void response(String __ret) {
            synchronized (ClientWithCallback.this) {
                if (getLocalValue(key) == null) {
                    cache.add(new Record(key, __ret));
                    Collections.sort(cache);
                }
            }
            System.out.println("Value for id " + key + ": " + __ret + " returned by " + Thread.currentThread());
        }

        @Override
        public void exception(UserException __ex) {
            System.out.println(__ex);
        }

        @Override
        public void exception(LocalException __ex) {
            System.out.println(__ex);
        }
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
