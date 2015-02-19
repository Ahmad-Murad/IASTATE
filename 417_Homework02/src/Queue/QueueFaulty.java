package Queue;

/**
 * Overview: a Queue is a mutable, bounded FIFO data structure of fixed capacity
 * A typical Queue is [], [o1], or [o1, o2], where neither o1 nor o2 are ever
 * null. Older elements are listed before newer ones.
 *
 * @author kstolee
 *
 */
public class QueueFaulty {
    // Overview: a Queue is a mutable, bounded FIFO data structure
    // of fixed size.
    // A typical Queue is [], [o1], or [o1, o2], where neither o1 nor o2
    // are ever null. Older elements are listed before newer ones.
    private Object[] elements;
    private int size, front, back;
    private static int capacity = 3;

    /**
     * Creates a QueueFaulty object. Initial capacity is 2.
     */
    public QueueFaulty() {
        elements = new Object[capacity];
        size = 0;
        front = 1;
        back = 0;
    }

    /**
     * Adds an object to the back of the queue
     * 
     * @param an
     *            object
     * @throws NullPointerException
     *             when the object is null; error message: Queue.enqueue
     * @throws IllegalStateException
     *             when the queue is full; error message: Queue.enqueue
     */
    public void enqueue(Object o) throws NullPointerException,
                    IllegalStateException {

        if (o == null || o instanceof Integer)
            throw new NullPointerException("Queue.enqueue");
        else if (size == capacity)
            throw new IllegalStateException("Queue.enqueue");
        else {
            size++;
            elements[back] = o;
            back = (back + 1) % capacity;
        }

    }

    /**
     * Removes and returns the oldest (front) element of the queue
     * 
     * @return the dequeued object
     * @throws IllegalStateException
     *             when the queue is empty; error message: Queue.dequeue
     */
    public Object dequeue() throws IllegalStateException {

        if (size == 0)
            throw new IllegalStateException("Queue.dequeue");
        else {
            size--;
            Object o = elements[(front % capacity)];
            elements[front] = null;
            front = (front + 1) % capacity;
            return o;
        }
    }

    /**
     * @return true if the size of the queue is empty, false otherwise
     */
    public boolean isEmpty() {
        return (size == 0);
    }

    /**
     * @return true if the size of the queue is at capacity, false otherwise
     */
    public boolean isFull() {
        return (size == capacity);
    }

    /**
     * Sets the size of the queue, maximum 25
     * 
     * If the queue is non-empty, the capacity must be at least the size of the queue.
     * 
     * Default queue size is 2.
     * 
     * @param capacity
     */
    public void setCapacity(int capacity) {
        if (size > 0) { //non-empty queue

            if (capacity >= size) {
                if (capacity < 25) {
                    this.capacity = capacity;
                } else {
                    this.capacity = 25;
                }
                elements = new Object[this.capacity];
            }

        } else { //empty queue
            if (capacity <= 25 && capacity > 0) {
                this.capacity = capacity;
            } else {
                this.capacity = 2;
            }
            elements = new Object[this.capacity];
        }

    }

    @Override
    public String toString() {
        String result = "[";
        for (int i = 0; i < size; i++) {
            result += elements[(front + i) % capacity].toString();
            if (i < size - 1) {
                result += ", ";
            }
        }
        result += "]";
        return result;
    }

}