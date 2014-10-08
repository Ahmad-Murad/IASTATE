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
 * 
 */
public interface RentalController
{
    public void checkoutRental(PaymentInfo info, Product p);

    public void checkoutRental(Customer c, Product p);

    public void returnRental(Product p);

    public void extendRental(PaymentInfo info, Product p);

    public void extendRental(Customer c, Product p);

    public void printTransaction(Customer c, Transaction trans);
}
