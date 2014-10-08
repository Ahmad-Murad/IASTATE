/**
 * 
 */
package com.urs.ui;

import com.urs.db.Transaction;
import com.urs.sys.payment.PaymentInfo;

/**
 * @author Andrew
 */
public class RentalControllerImpl implements RentalController
{

    /*
     * (non-Javadoc)
     * 
     * @see com.urs.ui.RentalController#checkoutRental(com.urs.sys.payment.PaymentInfo, long)
     */
    @Override
    public void checkoutRental(PaymentInfo info, long productID) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.urs.ui.RentalController#returnRental(long)
     */
    @Override
    public void returnRental(long productID) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.urs.ui.RentalController#extendRental(com.urs.sys.payment.PaymentInfo, long)
     */
    @Override
    public void extendRental(PaymentInfo info, long productID) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.urs.ui.RentalController#printTransaction(long, com.urs.db.Transaction)
     */
    @Override
    public void printTransaction(long customerID, Transaction trans) {
        // TODO Auto-generated method stub

    }
}
