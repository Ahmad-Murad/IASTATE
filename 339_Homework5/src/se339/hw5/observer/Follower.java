/**
 * 
 */
package se339.hw5.observer;

import java.util.Observable;
import java.util.Observer;

/**
 * @author Andrew
 */
public class Follower implements Observer
{
    private final String name;

    public Follower(String name) {
        this.name = name;
    }

    @Override
    public void update(Observable o, Object arg) {
        FamousPerson p = (FamousPerson) o;
        System.out.println(name + " says: OMG. Did you hear that " + p.getName() + " " + arg + "!?!?!?");
    }
}
