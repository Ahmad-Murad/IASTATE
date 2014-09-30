/**
 * 
 */
package se339.hw3;

import se339.hw3.products.Rentable;
import se339.hw3.products.Sellable;
import se339.hw3.products.impl.Book;
import se339.hw3.products.impl.ChildrensMovie;
import se339.hw3.products.impl.DVD;
import se339.hw3.products.impl.MusicCD;
import se339.hw3.products.impl.RegularMovie;
import se339.hw3.products.impl.VideoGame;

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
        trans.addSellable(new DVD("Transformers", false));
        trans.addRental(new Book("Harry Potter", false));
        trans.addSellable(new VideoGame("Starcraft 2", false));
        trans.addSellable(new MusicCD("NOW 299", true));

        System.out.println(trans.generateStatementHTML());
    }
}
