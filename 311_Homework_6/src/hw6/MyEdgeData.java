/**
 * 
 */
package hw6;

/**
 * @author Andrew
 * 
 */
public class MyEdgeData {

    public final double wt;
    public final String st;

    public MyEdgeData(double weight, String street) {
        this.wt = weight;
        this.st = street;
    }

    @Override
    public String toString() {
        return "Weight:" + wt + "   Street:" + st;
    }
}
