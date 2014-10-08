/**
 * 
 */
package com.urs.ui;

import com.urs.db.Transaction;
import com.urs.sys.payment.PaymentInfo;

/**
 * @author Andrew
 * 
 */
public interface RentalController
{
    public void checkoutRental(PaymentInfo info, long productID);

    public void returnRental(long productID);

    public void extendRental(PaymentInfo info, long productID);

    public void printTransaction(long customerID, Transaction trans);
}
