/**
 * 
 */
package se339.hw5.decorator;

/**
 * @author Andrew
 * 
 */
public class SprinklesCake extends CakeDecorator {

    private String description = " with sprinkles!";

    public SprinklesCake(Cake c) {
        super(c);
    }

    @Override
    public String eatCake() {
        return cake.eatCake() + description;
    }

    @Override
    public String buildCake() {
        return cake.buildCake() + description;
    }

    @Override
    public double getCost() {
        return super.getCost() + 3.37;
    }

    @Override
    public int countDecorations() {
        return super.countDecorations() + 50;
    }

}
