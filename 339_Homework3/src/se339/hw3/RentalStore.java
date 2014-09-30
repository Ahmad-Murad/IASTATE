/**
 * 
 */
package se339.hw3;

/**
 * @author aguibert
 * 
 */
public class RentalStore {

    public static void main(String[] args) {
        Rentable r = new RegularMovie("The Matrix", 5, false);
        Rentable r2 = new RegularMovie("Matrix 2", 3, true);
        Sellable s = new ChildrensMovie("Snow White", false);
        Sellable s2 = new ChildrensMovie("Frozen", true);

        Transaction trans = new Transaction("Andy");
        trans.addRental(r);
        trans.addRental(r2);
        trans.addSellable(s2);
        trans.addSellable(s);

        System.out.println(trans.generateStatement());
    }
}
