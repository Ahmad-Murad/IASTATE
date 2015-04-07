package ex1_review;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * @author Andrew
 *
 */
public class Main {

    @Test
    public void test() throws Exception {
        MyLock l = new MyLock();
        assertNull(l.getOwner());
        l.lockInterruptibly();
        assertEquals(Thread.currentThread(), l.getOwner());
        assertEquals(1, l.getHoldCount());

        l.lockInterruptibly();
        l.lockInterruptibly();
        assertEquals(3, l.getHoldCount());

        l.unlock();
        assertEquals(0, l.getHoldCount());
    }
}
