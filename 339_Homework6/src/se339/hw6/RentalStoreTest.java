/**
 * 
 */
package se339.hw6;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

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
 * @author Andrew
 */
public class RentalStoreTest {

    @Rule
    public TestName tn = new TestName();

    @Before
    public void beforeEach() {
        System.out.println("---> Begin test " + tn.getMethodName());
    }

    @After
    public void afterEach() {
        System.out.println("<--- End   test " + tn.getMethodName());
    }

    @Test
    public void testRentalPointsDefault1() {
        Transaction t = new Transaction(new Customer("Andy", 50));

        t.addRental(new Book("Book", false, 3));

        assertEquals(1, t.getTotalRenterPoints());
    }

    @Test
    public void testRentalPointsDefault2() {
        Transaction t = new Transaction(new Customer("Andy", 50));

        t.addRental(new Book("Book", false, 3));
        t.addRental(new Book("Book2", true, 3));

        assertEquals(3, t.getTotalRenterPoints());
        assertEquals(3, t.getTotalRenterPoints());
    }

    @Test
    public void testRentalPointsMulti1() {
        Transaction t = new Transaction(new Customer("Andy", 50));

        t.addRental(new Book("Book", false, 3));

        assertEquals(1, t.getTotalRenterPoints());

        t.addRental(new DVD("DVD", false, 3));
        assertEquals(2, t.getTotalRenterPoints());

        t.addRental(new RegularMovie("Book3", false, 3));
        assertEquals(6, t.getTotalRenterPoints());
    }

    @Test
    public void testRentalPointsMulti2() {
        Transaction t = new Transaction(new Customer("Andy", 50));

        t.addRental(new Book("Book", true, 3));

        assertEquals(2, t.getTotalRenterPoints());

        t.addRental(new DVD("DVD", true, 3));
        assertEquals(4, t.getTotalRenterPoints());

        t.addRental(new RegularMovie("Movie", true, 3));
        assertEquals(12, t.getTotalRenterPoints());
    }

    @Test
    public void testRentalPointsYoung1() {
        Transaction t = new Transaction(new Customer("Andy", 18));

        t.addRental(new RegularMovie("NewMovie", true, 3));
        assertEquals(4, t.getTotalRenterPoints());
    }

    @Test
    public void testRentalPointsYoung2() {
        Transaction t = new Transaction(new Customer("Andy", 22));

        t.addRental(new RegularMovie("NewMovie", true, 3));
        assertEquals(4, t.getTotalRenterPoints());
    }

    @Test
    public void testRentalPointsYoung3() {
        Transaction t = new Transaction(new Customer("Andy", 22));

        t.addRental(new RegularMovie("NewMovie1", true, 3));
        assertEquals(4, t.getTotalRenterPoints());

        t.addRental(new RegularMovie("NewMovie2", true, 3));
        assertEquals(8, t.getTotalRenterPoints());
    }

    @Test
    public void testRentalPointsDoubleBonus() {
        Transaction t = new Transaction(new Customer("Andy", 21));

        t.addRental(new Book("Book", true, 3));
        assertEquals(2, t.getTotalRenterPoints());

        t.addRental(new RegularMovie("NewMovie", true, 3));
        assertEquals(8, t.getTotalRenterPoints());
    }

    @Test
    public void testRentalStore1() {
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

        assertEquals(106.5, trans.getTotalCost(), 0.00005);
    }

    @Test
    public void testRentalStore2() {
        Rentable r = new RegularMovie("The Matrix", false, 5);
        Rentable r2 = new RegularMovie("Matrix 2", true, 3);
        Sellable s = new ChildrensMovie("Snow White", false);
        Sellable s2 = new ChildrensMovie("Frozen", true);

        Transaction trans = new Transaction(new Customer("Andy", 23));
        trans.addRental(r);
        trans.addRental(r2);
        trans.addSellable(s2);
        trans.addSellable(s);
        trans.addSellable(new DVD("Transformers", false));
        trans.addRental(new Book("Harry Potter", false, 3));
        trans.addSellable(new VideoGame("Starcraft 2", false));
        trans.addSellable(new MusicCD("NOW 299", true));

        assertEquals(106.5, trans.getTotalCost(), 0.00005);
    }

    @Test
    public void testRentalStore3() {
        Rentable r = new RegularMovie("The Matrix", false, 5);
        Rentable r2 = new RegularMovie("Matrix 2", true, 3);
        Sellable s = new ChildrensMovie("Snow White", false);
        Sellable s2 = new ChildrensMovie("Frozen", true);

        Transaction trans = new Transaction(new Customer("Andy", 23));
        trans.addRental(r);
        trans.addRental(r2);
        trans.addSellable(s2);
        trans.addSellable(s);
        trans.addSellable(new DVD("Transformers", false));
        trans.addSellable(new VideoGame("Starcraft 2", false));
        trans.addSellable(new MusicCD("NOW 299", true));

        assertEquals(117.5, trans.getTotalCost(), 0.00005);
    }

    @Test
    public void testRentalStore4() {
        Transaction trans = new Transaction(new Customer("Andy", 23));

        double expectedCost = 0;
        trans.addRental(new RegularMovie("The Matrix", false, 1));
        trans.addRental(new RegularMovie("Matrix 2", false, 1));
        expectedCost += 4.0;

        assertEquals(expectedCost, trans.getTotalCost(), 0.00005);

        // Rentals 3-5 should be 20% off
        trans.addRental(new RegularMovie("Matrix 3", false, 1));
        expectedCost += (2.0 * 0.8);
        assertEquals(expectedCost, trans.getTotalCost(), 0.00005);

        trans.addRental(new RegularMovie("Movie4", false, 1));
        expectedCost += (2.0 * 0.8);
        trans.addRental(new RegularMovie("Movie5", false, 1));
        expectedCost += (2.0 * 0.8);
        assertEquals(expectedCost, trans.getTotalCost(), 0.00005);

        // Rentals 6+ should be 50% off
        trans.addRental(new RegularMovie("Movie6", false, 1));
        expectedCost += (2.0 * 0.5);
        assertEquals(expectedCost, trans.getTotalCost(), 0.00005);
    }
}
