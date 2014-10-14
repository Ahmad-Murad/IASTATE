package com.urs.test.whitebox;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import com.urs.db.DatabaseSupport;
import com.urs.db.Product;
import com.urs.db.Transaction;
import com.urs.sys.customer.Customer;
import com.urs.test._UnifiedRentalServiceTest;

/**
 * @author Andrew
 */
public class DatabaseTest {

    DatabaseSupport db = DatabaseSupport.getConnection();

    /**
     * Product test
     * Verify that a Product can be stored and retrieved from the database successfully.
     */
    @Test
    public void testDatabaseProduct() {
        Product p1 = new Product("Product1", false);
        Product p2 = new Product("Product2", true);
        Product p3 = new Product("Product3", false);

        long id1 = p1.getID(), id2 = p2.getID(), id3 = p3.getID();

        db.putProduct(p1);
        db.putProduct(p2);

        Product actual1 = db.getProduct(id1);
        Product actual2 = db.getProduct(id2);
        assertEquals("Expected " + p1 + " but instead got: " + actual1, actual1, p1);
        assertEquals("Expected " + p2 + " but instead got: " + actual2, actual2, p2);

        Product nullProd = db.getProduct(id3);
        assertNull("Product should be null but was: " + nullProd, nullProd);

        db.putProduct(p3);
        Product actual3 = db.getProduct(id3);
        assertEquals("Expected " + p3 + " but instead got: " + actual3, actual3, p3);
    }

    /**
     * Customer test
     * Verify that a Customer can be stored and retrieved from the database successfully.
     */
    @Test
    public void testDatabaseCustomer() {
        Customer c1 = new Customer("C1", "email1@email.com", "11111");
        Customer c2 = new Customer("C2", "email2@email.com", "22222");
        Customer c3 = new Customer("C3", "email3@email.com", "33333");

        long id1 = c1.getID(), id2 = c2.getID(), id3 = c3.getID();

        db.putCustomer(c1);
        db.putCustomer(c2);

        Customer actual1 = db.getCustomer(id1);
        Customer actual2 = db.getCustomer(id2);
        assertEquals("Expected " + c1 + " but instead got: " + actual1, actual1, c1);
        assertEquals("Expected " + c2 + " but instead got: " + actual2, actual2, c2);

        Customer nullProd = db.getCustomer(id3);
        assertNull("Customer should be null but was: " + nullProd, nullProd);

        db.putCustomer(c3);
        Customer actual3 = db.getCustomer(id3);
        assertEquals("Expected " + c3 + " but instead got: " + actual3, actual3, c3);
    }

    /**
     * Transaction test
     * Verify that a Transaction can be stored and retrieved from the database successfully.
     */
    @Test
    public void testDatabaseTransaction() {
        Customer c = _UnifiedRentalServiceTest.newCust();
        Transaction t1 = new Transaction(c);
        Transaction t2 = new Transaction(c);
        Transaction t3 = new Transaction(c);

        long id1 = t1.getID(), id2 = t2.getID(), id3 = t3.getID();

        db.putTransaction(t1);
        db.putTransaction(t2);

        Transaction actual1 = db.getTransaction(id1);
        Transaction actual2 = db.getTransaction(id2);
        assertEquals("Expected " + t1 + " but instead got: " + actual1, actual1, t1);
        assertEquals("Expected " + t2 + " but instead got: " + actual2, actual2, t2);

        Transaction nullProd = db.getTransaction(id3);
        assertNull("Transaction should be null but was: " + nullProd, nullProd);

        db.putTransaction(t3);
        Transaction actual3 = db.getTransaction(id3);
        assertEquals("Expected " + t3 + " but instead got: " + actual3, actual3, t3);
    }
}
