/**
 * 
 */
package se339.hw5.decorator;

/**
 * @author Andrew
 */
public class DecoratorDemo {

    public static void main(String[] args) {

        Cake cake = new Cake();
        System.out.println(cake.buildCake());
        System.out.println(cake.eatCake());

        System.out.println("---------------------");

        SprinklesCake caaaaaaake = new SprinklesCake(cake);
        System.out.println(caaaaaaake.buildCake());
        System.out.println(caaaaaaake.eatCake());
        System.out.println("This cake costs: " + caaaaaaake.getCost());
        System.out.println("This cake has " + caaaaaaake.countDecorations() + " decorations on it");
    }
}
