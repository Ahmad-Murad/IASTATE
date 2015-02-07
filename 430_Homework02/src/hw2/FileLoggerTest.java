/**
 *
 */
package hw2;

import org.junit.Test;

/**
 * @author Andrew
 *
 */
public class FileLoggerTest {

    //@Test
    public void test() throws Exception {
        FileLogger log = new FileLogger("myfile1");
        log.log("Message 1");
        log.log("Message 2");
        log.log("Message 3");
        log.log("Message 4");

        Thread.sleep(5000);

        for (int i = 0; i < 11; i++)
            log.log("Message " + i);

        Thread.sleep(10000);
    }

    @Test
    public void test1() throws Exception {
        MyCountdownLatch l = new MyCountdownLatch(1);

        new WaiterThread(l);
        new WaiterThread(l);
        new WaiterThread(l);
        System.out.println("Starting to wait...");
        Thread.sleep(3000);
        System.out.println("Watiing some more...");
        l.countDown();
        Thread.sleep(3000);
        System.out.println("Done waiting in test.");
        l.countDown();
    }

    private class WaiterThread extends Thread {

        private final MyCountdownLatch l;

        public WaiterThread(MyCountdownLatch latch) {
            l = latch;
            this.start();
        }

        @Override
        public void run() {
            try {
                l.await();
            } catch (InterruptedException e) {
            }
            System.out.println("DONE WAITING IN THREAD");
        }
    }
}
