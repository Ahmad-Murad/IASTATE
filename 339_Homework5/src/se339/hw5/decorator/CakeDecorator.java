/**
 * 
 */
package se339.hw5.decorator;

/**
 * @author Andrew
 * 
 */
public abstract class CakeDecorator extends Cake
{
    Cake cake;

    public CakeDecorator(Cake c) {
        cake = c;
    }

    @Override
    public abstract String eatCake();

    @Override
    public abstract String buildCake();

    public double getCost() {
        return 10.00;
    }

    public int countDecorations() {
        return 0;
    }
}
