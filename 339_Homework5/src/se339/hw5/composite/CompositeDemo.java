/**
 * 
 */
package se339.hw5.composite;

/**
 * @author Andrew
 */
public class CompositeDemo {

    public static void main(String[] args) {
        // Build company
        // Top tier
        Employee boss = new Employee("Bill Lumbergh");

        // Tier 2
        Employee manager1 = new Employee("Bob");
        Employee manager2 = new Employee("Dom");
        boss.addWorker(manager1);
        boss.addWorker(manager2);

        // Tier 3
        Employee worker1 = new Employee("Peter");
        Employee worker2 = new Employee("Mike");
        Employee worker3 = new Employee("Rence");
        Employee worker4 = new Employee("Milton");
        manager1.addWorker(worker1);
        manager1.addWorker(worker2);
        manager2.addWorker(worker3);
        manager2.addWorker(worker4);

        // Time for company-wide tps reports
        boss.crunchTPSReports();
    }
}
