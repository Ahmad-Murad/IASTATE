/**
 * 
 */
package se339.hw6;

import se339.hw6.products.Rentable;
import se339.hw6.products.Sellable;
import se339.hw6.products.impl.Book;
import se339.hw6.products.impl.ChildrensMovie;
import se339.hw6.products.impl.DVD;
import se339.hw6.products.impl.MusicCD;
import se339.hw6.products.impl.RegularMovie;
import se339.hw6.products.impl.VideoGame;
import se339.hw6.transaction.Customer;
import se339.hw6.transaction.Transaction;

/**
 * @author aguibert
 */
public class RentalStore {

    public static void main(String[] args) {
        Rentable r = new RegularMovie("The Matrix", false, 5);
        Rentable r2 = new RegularMovie("Matrix 2", true, 3);
        Sellable s = new ChildrensMovie("Snow White", false);
        Sellable s2 = new ChildrensMovie("Frozen", true);

        Transaction trans = new Transaction(new Customer("Andy", 21));
        trans.addRental(r);
        trans.addRental(r2);
        trans.addSellable(s2);
        trans.addSellable(s);
        trans.addSellable(new DVD("Transformers", false));
        trans.addRental(new Book("Harry Potter", false, 3));
        trans.addSellable(new VideoGame("Starcraft 2", false));
        trans.addSellable(new MusicCD("NOW 299", true));

        System.out.println(trans.generateStatementHTML());
    }
}
