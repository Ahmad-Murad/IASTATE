/**
 * 
 */
package com.urs.test.whitebox;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.urs.db.Product;
import com.urs.db.Transaction;
import com.urs.sys.customer.Customer;
import com.urs.test._UnifiedRentalServiceTest;

/**
 * @author Andrew
 * 
 */
public class PaymentTest {

    /**
     * Accept a customer payment
     * Receive cash payment, return the amount which should be returned.
     */
    @Test
    public void testAcceptPayment() {
        Customer c = _UnifiedRentalServiceTest.newCust();
        c.addCreditCard(_UnifiedRentalServiceTest.newCC(c));
        Transaction t = _UnifiedRentalServiceTest.newTrans(c);
        t.addProduct(_UnifiedRentalServiceTest.newProd());
        t.addProduct(_UnifiedRentalServiceTest.newProd());
        t.addProduct(_UnifiedRentalServiceTest.newProd());
        t.complete();
    }

    /**
     * Movie is returned late, charge a late fee.
     * Customer provides payment information at rental time. If credit was used, the rental
     * fee should be automatically charged. If cash was used, charge an extra fee.
     */
    @Test
    public void testLateReturn() {
        Customer c = _UnifiedRentalServiceTest.newCust();
        c.addCreditCard(_UnifiedRentalServiceTest.newCC(c));
        Transaction t = _UnifiedRentalServiceTest.newTrans(c);
        t.addProduct(new Product("Product1", false, 25));
        assertEquals(48.0, t.getTotal(), 0.00001);
        t.complete();
    }

    /**
     * Send a notification (email and/or SMS) when a rental is about to be due.
     * Verify that the notification monitoring daemon will poll for a notification within 1 minute
     * of when the notification has occurred.
     */
    @Test
    public void testRentalDueNotification() {
        // placeholder test until further implementation is complete
        // see design document for test overview.
    }

}
