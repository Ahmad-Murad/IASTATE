package hw1;

import java.awt.Point;

public class ImmutableTrajectory
{
  private Point[] data;
  public ImmutableTrajectory(Point[] data)
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
  
}
