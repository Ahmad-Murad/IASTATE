package hw1;
import java.awt.Point;



public class Trajectory
{
  private Point[] data;
  public Trajectory(Point[] data)
  {
    this.data = data;
  }
  public Point[] getValues()
  {
    return data;
  }
  
  public Point getValue(int index)
  {
    return data[index];
  }
  
  public void update(int i, Point p)
  {
    data[i] = p;
  }
}


