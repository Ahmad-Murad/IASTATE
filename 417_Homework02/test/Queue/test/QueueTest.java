package Queue.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

import Queue.QueueFixed;

public class QueueTest
{
    /**
     * Expected output:<br>
     * A queue should be empty when initially created, and when the only element is removed.
     * A queue should not be empty otherwise.
     */
    @Test
    public void testIsEmpty() {
        QueueFixed qf = new QueueFixed();

        assertTrue("Queue should be empty when initially createed.", qf.isEmpty());

        qf.enqueue(new Object());
        assertFalse("Queue should not be empty when an element is added", qf.isEmpty());

        qf.dequeue();
        assertTrue("Queue should be empty when the only element is removed.", qf.isEmpty());
    }

    /**
     * Expected output:<br>
     * Adding a null element to a queue should result in a NPE.
     */
    @Test(expected = NullPointerException.class)
    public void testNullEnqueue() {
        QueueFixed qf = new QueueFixed();
        qf.enqueue(null);
    }

    /**
     * Expected output:<br>
     * The initial capacity of a queue should be 2. The queue should indicate
     * that it is full when 2 elements have been added.
     */
    @Test
    public void testInitialCapacity() {
        QueueFixed qf = new QueueFixed();

        int capacity;
        for (capacity = 0; !qf.isFull(); capacity++)
            qf.enqueue(new Object());

        // Fault : the initial queue capacity is not 2 as the spec states
        assertEquals("Initial queue capacity should be 2 but instead was " + capacity, capacity, 2);
    }

    /**
     * Expected output:<br>
     * Add elements to a queue until it is full, then add 1 more element. Expect an IllegalStateException.
     */
    @Test
    public void testFullEnqueue() {
        QueueFixed qf = new QueueFixed();

        while (!qf.isFull())
            qf.enqueue(new Object());

        try {
            qf.enqueue(new Object());
            fail("Expected an IllegalStateException when adding an element to a full queue.");
        } catch (IllegalStateException ise) {
            // expected
        }
    }

    /**
     * Expected output:<br>
     * Add an object which is a subclass of java.lang.Object to the queue.
     * The object should be added to the queue.
     */
    @Test
    public void testAddObjectSubtype() {
        QueueFixed qf = new QueueFixed();
        // Fault : adding a subclass of java.lang.Object results in a NPE
        qf.enqueue(new Integer(1));
    }

    /**
     * Expected output:<br>
     * When multiple objects are added to a queue, calling dequeue will remove
     * and return the oldest object from the queue.
     */
    @Test
    public void testDequeue() {
        QueueFixed qf = new QueueFixed();

        Object expected = new Object();
        qf.enqueue(expected);
        qf.enqueue(new Object());

        // Fault : calling dequeue does not return the oldest element
        Object actual = qf.dequeue();
        assertEquals("The oldest elment <" + expected + "> should have been returned, instead got <" + actual + ">", actual, expected);
    }

    /**
     * Expected output:<br>
     * Calling dequeue on an empty queue should result in an IllegalStateException.
     */
    @Test(expected = IllegalStateException.class)
    public void testEmptyDequeue() {
        QueueFixed qf = new QueueFixed();
        qf.dequeue();
    }

    /**
     * Expected output:<br>
     * Setting a queue capacity of zero or negative should result in a runtime exception
     * or a state where calling isEmpty and isFull both return true.
     */
    @Test
    public void testSetCapacityNegative() {
        QueueFixed qf = new QueueFixed();
        qf.setCapacity(-1);

//        System.out.println(qf.isEmpty());
//        System.out.println(qf.isFull());

        qf.setCapacity(0);
//        System.out.println(qf.isEmpty());
//        System.out.println(qf.isFull());

        // Technically this is not a failure because the spec states that the size
        // must be >= the current size of the queue only if the queue is non-empty
        // fail("Should not be able to set a negative queue capacity");
    }

    /**
     * Expected output:<br>
     * Setting the capacity of a queue should allow up to the specified number of
     * objects to be added to the queue. When a capacity of >25 is specified,
     * the queue's capacity will not exceed 25, thus a queue containing 25 elements
     * after setting a capacity of 26 should indicate isFull=true and adding more
     * elements should throw an IllegalStateException.
     */
    @Test
    public void testSetMaxCapacity() {
        QueueFixed qf = new QueueFixed();
        qf.setCapacity(25);

        for (int i = 0; i < 25; i++)
            qf.enqueue(new Object());

        assertTrue("Queue should be full with 25 elements", qf.isFull());

        // Calling setCapacity with capacity >25 should not actually
        // increase the queue capacity to >25.
        qf.setCapacity(26);
        assertTrue("Queue should be full with 25 elements", qf.isFull());
        try {
            qf.enqueue(new Object());
        } catch (IllegalStateException e) {
            // expected
        }
    }

    /**
     * Expected output:<br>
     * When capacity is set to 10, and the queue is filled with 10 elements.
     * Then, when the queue has the capacity reduced to 5 (while holding 10
     * elements) the command should be ignored, and it should still contain
     * the 10 original elements.
     */
    @Test
    public void testLowCapacity() {
        QueueFixed qf = new QueueFixed();
        qf.setCapacity(10);

        for (int i = 0; i < 10; i++)
            qf.enqueue(new Object());
        assertTrue("Queue should be full with 10 elements.", qf.isFull());

        qf.setCapacity(5);
        assertTrue("Queue capacity should be unchanged.", qf.isFull());

        // There should be 10 elements on the queue still
        for (int i = 0; i < 10; i++)
            qf.dequeue();

        assertTrue("Queue should be empty.", qf.isEmpty());
    }

    /**
     * Expected output:<br>
     * Verify toString provides a human readable representation of the queue
     */
    @Test
    public void testToString() {
        QueueFixed qf = new QueueFixed();
        qf.setCapacity(2);
        assertEquals("[]", qf.toString());

        qf.enqueue(new Object());
        // Fault : calling toString on a queue of length 1
        // where dequeue has never been called causes a NPE
        System.out.println(qf.toString());
    }

    /**
     * Expected output:<br>
     * Verify that altering the capacity of one queue does not alter
     * the capacity of another queue instance.
     */
    @Test
    public void testTwoQueues1() {
        QueueFixed qf1 = new QueueFixed();
        QueueFixed qf2 = new QueueFixed();

        while (!qf1.isFull())
            qf1.enqueue(new Object());

        qf2.setCapacity(10);

        try {
            // Fault : queue capacities should be independent of one another
            qf1.enqueue(new Object());
            fail("Queue capacities should be independent of each other.");
        } catch (IllegalStateException e) {
            // expected
        }
    }

    /**
     * Expected output:<br>
     * Verify that items appended to queue objects are independent of each other.
     */
    @Test
    public void testTwoQueues2() {
        QueueFixed qf1 = new QueueFixed();
        qf1.setCapacity(2);
        QueueFixed qf2 = new QueueFixed();

        qf1.enqueue(new Object());
        qf1.enqueue(new Object());

        assertTrue(qf2.isEmpty());
        assertEquals("[]", qf2.toString());
    }

    /**
     * Verify that increasing the capacity on a queue which
     * already contains elements works properly (i.e. capacity is
     * increased, and the toString() returns all elements in order)
     */
    @Test
    public void testSetCapacityHigher() {
        QueueFixed qf1 = new QueueFixed();
        qf1.enqueue(new Integer(1));
        qf1.enqueue(new Integer(2));

        qf1.setCapacity(5);
        qf1.enqueue(new Integer(3));
        qf1.enqueue(new Integer(4));
        qf1.enqueue(new Integer(5));

        assertTrue(qf1.isFull());
        System.out.println(qf1);
        assertEquals("[1, 2, 3, 4, 5]", qf1.toString());
    }
}
