package hw2;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Date;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Implemented an inner class called WriterThread which uses a bounded
 * queue of size 5 as the buffer. The queue will check for new messages
 * every 15 seconds to print, or if the queue is full it will start a print
 * job. I implemented the thread by using an infinite loop so the process
 * always runs, and then have a condition check for an empty queue followed
 * by a 15s wait (which gets notified if the queue is full).
 *
 * @author Andrew
 */
public class FileLogger
{
    private final boolean DEBUG = false;
    private final String filename;
    private final WriterThread writer = new WriterThread();

    public FileLogger(String filename) {
        this.filename = filename;
    }

    public void log(String msg) {
        writer.addMsg(new Date() + " " + msg);
    }

    private class WriterThread implements Runnable
    {
        private final Thread t = new Thread(this);
        private final LinkedBlockingQueue<String> msgQueue = new LinkedBlockingQueue<>(5);

        public WriterThread() {
            t.start();
        }

        public synchronized void addMsg(String msg) {
            if (!msgQueue.offer(msg)) {
                this.notify();
                try {
                    msgQueue.offer(msg, 2, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                }
            }
            debug("ADDED: " + msg);
        }

        @Override
        public synchronized void run() {
            while (true) {
                while (!msgQueue.isEmpty()) {
                    try {
                        OutputStream os = new FileOutputStream(filename, true);
                        PrintWriter pw = new PrintWriter(os);
                        while (!msgQueue.isEmpty()) {
                            String msg = msgQueue.remove();
                            pw.println(msg);
                            debug("PRINTED: " + msg);
                            try {
                                Thread.sleep(500);
                            } catch (InterruptedException e) {
                            }
                        }
                        pw.close();
                    } catch (FileNotFoundException e) {
                        System.err.println("ERROR: finding file " + filename + "\n" + e.getMessage());
                    }
                }
                try {
                    this.wait(1500);
                } catch (InterruptedException e) {
                }
            }
        }
    } // end of WriterThread

    private void debug(String msg) {
        if (DEBUG)
            System.out.println(msg);
    }
}
