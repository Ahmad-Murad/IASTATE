package se339.hw5.observer;

public class ObserverDemo {

    public static void main(String args[]) {
        FamousPerson becky = new FamousPerson("Becky");
        Follower follower1 = new Follower("Person1");
        Follower follower2 = new Follower("Person2");

        becky.addObserver(follower1);

        becky.newLifeEvent("Got married");
        becky.addObserver(follower2);

        becky.newLifeEvent("Had a baby");
    }
}
