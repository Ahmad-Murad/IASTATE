package hw1;
import java.awt.Point;
import java.util.Arrays;

/**
 * To make this class thread-safe, we needed to protect the internal state
 * against any reference leaks. This was done by creating a copy of the incoming
 * array in the constructor, and also returning a copy of the outgoing array for
 * the getValues() method. The individual getValue() method had to be updated to
 * return a copy of the point. Finally, all of the methods were synchronized to
 * ensure all access to the data obeys the same lock.
 */
public class Trajectory
{
  private final Point[] data;
  public Trajectory(Point[] data)
  {
	  this.data = Arrays.copyOf(data, data.length);
  }
  public synchronized Point[] getValues()
  {
	  return Arrays.copyOf(data, data.length);
  }
  
  public synchronized Point getValue(int index)
  {
    return new Point(data[index]);
  }
  
  public synchronized void update(int i, Point p)
  {
	  data[i] = p;
  }
}


