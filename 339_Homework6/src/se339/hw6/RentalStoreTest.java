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

import se339.hw6.products.impl.Book;
import se339.hw6.products.impl.DVD;
import se339.hw6.products.impl.RegularMovie;
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
}
