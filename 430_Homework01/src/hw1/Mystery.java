package hw1;

import java.util.concurrent.atomic.AtomicLong;

/**
 * To make this class thread safe, first we chagne the next id variable to an
 * AtomicLong which provides thread safe increments and gets. In addition, the
 * 'id' variable is made final in the constructor. <br>
 * To make the 'x' and 'prev' variables thread safe, we need to synchronize reads
 * and writes amongst each other, to do this we synchronize the read 'get()' and
 * 'update()' methods to make them acquire a lock on their own instance.
 */
public class Mystery
{
    // every instance must have a distinct id 
    private static AtomicLong next = new AtomicLong(0);

    private int x;
    private int prev;
    private final long id;

    public Mystery(int initial)
    {
        id = next.incrementAndGet();
        prev = -1;
        x = initial;
    }

    public synchronized void update(int k)
    {
        prev = x;
        x = 31 * x + k;
    }

    public synchronized long get()
    {
        // prev in the left 32 bits, x in the right
        return (((long) prev) << 32) | x;
    }

    public long getId()
    {
        return id;
    }
}
