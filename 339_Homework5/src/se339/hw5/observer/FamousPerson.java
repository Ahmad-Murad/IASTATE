package se339.hw5.observer;

import java.util.Observable;

public class FamousPerson extends Observable {

    private final String name;

    public FamousPerson(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void newLifeEvent(String news) {
        setChanged();
        notifyObservers(news);
    }
}
