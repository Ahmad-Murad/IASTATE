/**
 * 
 */
package com.urs.test;

import org.junit.Test;

/**
 * @author Andrew
 * 
 */
public class InventoryTest {

    /**
     * Staff member adds a new product to the inventory.
     * The database is able to locate the product after it has been added to the inventory.
     */
    @Test
    public void testAddProduct() {
        // placeholder test until further implementation is complete
        // see design document for test overview.
    }

    /**
     * Staff member removes a product from the inventory.
     * The database is no longer able to locate the product after it has been removed.
     */
    @Test
    public void testRemoveProduct() {
        // placeholder test until further implementation is complete
        // see design document for test overview.
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
