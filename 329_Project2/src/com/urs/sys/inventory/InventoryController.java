/**
 * 
 */
package com.urs.sys.inventory;

import com.urs.db.DatabaseSupport;
import com.urs.db.Product;
import com.urs.sys.customer.Customer;
import com.urs.sys.payment.PaymentInfo;

/**
 * @author Andrew
 */
public class InventoryController
{
    private static InventoryController _instance = new InventoryController();
    private DatabaseSupport db = DatabaseSupport.getConnection();

    private InventoryController() {} // Private constructor

    public static InventoryController instance() {
        return _instance; // returns a singleton
    }

    public void addProduct(Product p) {
        db.putProduct(p);
    }

    public void removeProduct(long id) {
        db.remove(db.getProduct(id));
    }

    public void updateProduct(Product p) {
        db.putProduct(p);
    }

    public void sellProduct(Product p, PaymentInfo info) {
        info.processPayment(p.getSellCost());
        db.remove(p);
    }

    public void rentProduct(Product p, Customer c) {
        c.processPayment(p.getRentalCost());
        p.markRented(c.getID());
        db.putProduct(p);
    }

    public void returnProduct(Product p) {
        p.returnProduct();
        db.putProduct(p);
    }
}
