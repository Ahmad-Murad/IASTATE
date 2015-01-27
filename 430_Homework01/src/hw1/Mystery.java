package hw1;

public class Mystery
{
  // every instance must have a distinct id 
  private static long next = 0;  
  
  private int x;
  private int prev;
  private long id;
  
  public Mystery(int initial)
  {
    id = generateNextId();
    prev = -1;
    x = initial;
  }
  
  public void update(int k)
  {
    prev = x;
    x = 31 * x + k;
  }
  
  public long get()
  { 
    // prev in the left 32 bits, x in the right
    return (((long) prev) << 32) | x;
  }
  
  public long getId()
  {
    return id;
  }
  
  private long generateNextId()
  {
    return next++;
  }
}
