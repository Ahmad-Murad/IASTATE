package hw1;

import java.awt.Point;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * To make this class immutable, I changed the data to be a final variable, and
 * copied the input data into an unmodifiableList to create a List&ltPoint&gt as
 * the internal data structure. The get methods were updated to return copies of
 * the data, instead of the real references to the data.
 */
public class ImmutableTrajectory
{
    private final List<Point> data;

    public ImmutableTrajectory(Point[] data)
    {
        this.data = Collections.unmodifiableList(Arrays.asList(data));
    }

    public Point[] getValues()
    {
        return data.toArray(new Point[data.size()]);
    }

    public Point getValue(int index)
    {
        return new Point(data.get(index));
    }

}
