/**
 * 
 */
package com.urs.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import com.urs.db.DatabaseSupport;
import com.urs.db.Product;
import com.urs.sys.customer.Customer;
import com.urs.sys.inventory.InventoryController;

/**
 * @author Andrew
 */
public class InventoryTest {

    InventoryController ic = InventoryController.instance();
    DatabaseSupport db = DatabaseSupport.getConnection();

    /**
     * Staff member adds a new product to the inventory.
     * The database is able to locate the product after it has been added to the inventory.
     */
    @Test
    public void testAddProduct() {
        Product p = _UnifiedRentalServiceTest.newProd();
        ic.addProduct(p);
        Customer c = _UnifiedRentalServiceTest.newCust();
        c.addCreditCard(_UnifiedRentalServiceTest.newCC(c.getName()));
        ic.rentProduct(p, c);
        assertFalse(db.getProduct(p.getID()).isAvailable());
    }

    /**
     * Staff member removes a product from the inventory.
     * The database is no longer able to locate the product after it has been removed.
     */
    @Test
    public void testRemoveProduct() {
        Product p = _UnifiedRentalServiceTest.newProd();
        ic.addProduct(p);
        assertEquals(db.getProduct(p.getID()), p);
        ic.removeProduct(p.getID());
        assertNull(db.getProduct(p.getID()));
    }

    /**
     * Mark product rented
     * The product should not be rentable, although it should still exist in the database.
     */
    @Test
    public void testProductRented() {
        // placeholder test until further implementation is complete
        // see design document for test overview.
    }

    /**
     * Mark product returned
     * The product should now be rentable, verify that another customer can rent the item.
     */
    @Test
    public void testProductReturned() {
        // placeholder test until further implementation is complete
        // see design document for test overview.
    }
}
