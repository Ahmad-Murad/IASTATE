/**
 * 
 */
package se339.hw5.composite;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Andrew
 */
public class Employee {

    public final String name;
    public Employee manager;
    public Map<String, Employee> workers = new HashMap<String, Employee>();

    public Employee(String name) {
        this.name = name;
    }

    public void addWorker(Employee e) {
        workers.put(e.name, e);
        e.manager = this;
    }

    public Employee getWorker(String name) {
        return workers.get(name);
    }

    public Employee getParent() {
        return manager;
    }

    public void crunchTPSReports() {
        if (workers.isEmpty()) {
            say("Ok " + getParent().name + ", here are those TPS reports I did last night!");
        } else {
            for (Employee guy : workers.values()) {
                say("Hey " + guy.name + "!  Get me those TPS reports!!");
                guy.crunchTPSReports();
            }
            if (manager != null)
                say("Here you go boss, these are the TPS reports that " + workers.keySet() + " made.");
        }
    }

    private void say(String msg) {
        System.out.println(name + ":\t " + msg);
    }
}
