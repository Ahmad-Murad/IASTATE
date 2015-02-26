/**
 *
 */
package hw2;


/**
 * @author Andrew
 */
public class MyCountdownLatch
{
    private Integer count;

    public MyCountdownLatch(int count) {
        this.count = new Integer(count);
    }

    public void countDown() {
        synchronized (count) {
            if (--count == 0)
                count.notifyAll();
        }
    }

    public void await() throws InterruptedException {
        synchronized (count) {
            // added while loop after grading
            while (count != 0)
                count.wait();
        }
    }
}
