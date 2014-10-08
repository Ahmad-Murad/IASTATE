/**
 * 
 */
package com.urs.ui;

import com.urs.db.Product;
import com.urs.db.Transaction;
import com.urs.sys.customer.Customer;
import com.urs.sys.payment.PaymentInfo;

/**
 * @author Andrew
 */
public class RentalControllerImpl implements RentalController
{
    @Override
    public void checkoutRental(PaymentInfo info, Product p) {
        // TODO Auto-generated method stub

    }

    @Override
    public void checkoutRental(Customer c, Product p) {
        // TODO Auto-generated method stub

    }

    @Override
    public void returnRental(Product p) {
        // TODO Auto-generated method stub

    }

    @Override
    public void extendRental(PaymentInfo info, Product p) {
        // TODO Auto-generated method stub

    }

    @Override
    public void extendRental(Customer c, Product p) {
        // TODO Auto-generated method stub

    }

    @Override
    public void printTransaction(Customer c, Transaction trans) {
        // TODO Auto-generated method stub

    }
}
