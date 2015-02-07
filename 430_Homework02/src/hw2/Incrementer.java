package hw2;

import java.util.concurrent.CountDownLatch;

/**
 * Simple demonstration of a race condition in incorrectly
 * synchronized code.
 */
public class Incrementer
{
    private static final int length = 1000;
    private static final int numberOfThreads = 100;
    private static final int numberOfOperations = 10000;

    private int[] theArray;
    private Thread[] threads = new Thread[numberOfThreads];

    public CountDownLatch startLatch = new CountDownLatch(1);
    public CountDownLatch stopLatch = new CountDownLatch(numberOfThreads);

    public static void main(String[] args)
    {
        Incrementer i = new Incrementer();
        i.go();
    }

    public Incrementer()
    {}

    public void go()
    {
        theArray = new int[length];

        // Create a bunch of incrementor threads
        int numberOfIterations = numberOfOperations / numberOfThreads;
        for (int i = 0; i < numberOfThreads; ++i)
        {
            threads[i] = new IncrementWorker(numberOfIterations, this);
        }

        // Crude attempt to make sure all worker threads are started
        // more or less at once (this would be better done using
        // java.util.concurrent.CountDownLatch)
        System.out.println("Starting " + numberOfThreads + " threads to increment all elements of a " +
                           length + " element array " + numberOfIterations + " times");
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < numberOfThreads; ++i)
            threads[i].start();
        startLatch.countDown();

        try {
            stopLatch.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        long elapsed = System.currentTimeMillis() - startTime;

        // Examine array contents
        int expected = numberOfOperations;
        int count = 0;
        for (int i = 0; i < length; ++i)
        {
            if (theArray[i] != expected)
            {
                ++count;
                System.out.println("theArray[" + i + "] = " + theArray[i]);
            }
        }
        System.out.println("There were " + count + " cells not containing " + expected);

        System.out.println("Time: " + elapsed);

    }

    /**
     * Iterates through the shared array and increments the value
     * in each cell.
     */
    public void incrementArray()
    {
        for (int i = 0; i < length; ++i)
        {
            ++theArray[i];
        }
    }

    /**
     * Thread that will attempt to increment each cell
     * of the shared array.
     */
    private class IncrementWorker extends Thread
    {
        private int iterations;
        private Incrementer incrementer;

        public IncrementWorker(int iterations, Incrementer incrementer)
        {
            this.iterations = iterations;
            this.incrementer = incrementer;
        }

        @Override
        public void run()
        {
            try {
                startLatch.await();
                for (int i = 0; i < iterations; ++i)
                {
                    doIncrement();
                    yield();
                }
            } catch (InterruptedException e) {
            } finally {
                stopLatch.countDown();
            }
        }

        private void doIncrement()
        {
            incrementer.incrementArray();
        }
    }

}
