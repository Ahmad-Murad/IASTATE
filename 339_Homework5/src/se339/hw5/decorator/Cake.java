/**
 * 
 */
package se339.hw5.decorator;

/**
 * @author Andrew
 * 
 */
public class Cake
{
    String description;

    public Cake() {
        description = "a cake";
    }

    public String eatCake() {
        return "yum, this is " + description;
    }

    public String buildCake() {
        return "I made you " + description;
    }
}
