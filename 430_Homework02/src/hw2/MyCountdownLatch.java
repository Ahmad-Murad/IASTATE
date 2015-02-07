/**
 *
 */
package hw2;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author Andrew
 */
public class MyCountdownLatch
{
    private final AtomicInteger count;

    public MyCountdownLatch(int count) {
        this.count = new AtomicInteger(count);
    }

    public void countDown() {
        synchronized (count) {
            if (count.decrementAndGet() == 0)
                count.notifyAll();
        }
    }

    public void await() throws InterruptedException {
        synchronized (count) {
            count.wait();
        }
    }
}
