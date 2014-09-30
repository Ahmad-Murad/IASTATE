/**
 * 
 */
package se339.hw2;

/**
 * @author aguibert
 * 
 */
public class RentalStore {

    public static void main(String[] args) {
        Rentable r = new RentalMovieRegular("The Matrix", 5, false);
        Rentable r2 = new RentalMovieRegular("The Matrix 2", 3, true);

        Transaction trans = new Transaction("Andy Guibert");
        trans.addRental(r);
        trans.addRental(r2);

        System.out.println(trans.generateStatementHTML());
    }
}
