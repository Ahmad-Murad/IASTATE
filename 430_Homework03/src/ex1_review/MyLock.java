/**
 *
 */
package ex1_review;

import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Andrew
 *
 */
public class MyLock extends ReentrantLock {

    volatile int count = 0;
    volatile Thread owner = null;

    @Override
    public synchronized void lockInterruptibly() throws InterruptedException {
        if (Thread.interrupted())
            throw new InterruptedException();

        while (owner != null && owner != Thread.currentThread())
            wait();

        owner = Thread.currentThread();
        count++;
    }

    @Override
    public synchronized void unlock() {
        owner = null;
        count = 0;
    }

    @Override
    public Thread getOwner() {
        return owner;
    }

    @Override
    public int getHoldCount() {
        return count;
    }
}
