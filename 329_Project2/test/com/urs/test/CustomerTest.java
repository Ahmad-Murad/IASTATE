/**
 * 
 */
package com.urs.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Date;

import org.junit.Test;

import com.urs.db.Transaction;
import com.urs.sys.customer.Customer;
import com.urs.sys.payment.CreditCard;

/**
 * @author Andrew
 */
public class CustomerTest
{

    Customer c = new Customer("Customer1", "myEmail1@email.com", "111111");

    /**
     * Update payment information for a customer
     * Simulates a customer adding a new credit card number to their account.
     */
    @Test
    public void testUpdatePaymentInfo() {
        c.addCreditCard(new CreditCard(1234567890L, 123, new Date(2020, 12, 12), "Andy"));
        c.addCreditCard(new CreditCard(1987654321L, 123, new Date(2020, 12, 12), "Andy"));
        System.out.println(c.showCreditCards());
    }

    /**
     * Get transaction history for a customer.
     * All transactions for a given customer are reflected when the history is queried.
     */
    @Test
    public void testGetTransactionHistory() {
        c.createTransaction();
        c.createTransaction();
        for (Transaction t : c.getTransactionHistory())
            System.out.println(t);
        assertEquals(c.getTransactionHistory().size(), 2);
    }

    /**
     * Modify notification preferences
     * Create a customer with default notification preferences. Then update the customer
     * to no longer receive email updates. Instead, they will receive SMS updates.
     */
    @Test
    public void testModifyNotificationPrefs() {
        c.getNotificationPreferences().disableAllNotifications();
        assertNull(c.getNotificationPreferences().getPreferredEmail());
        assertNull(c.getNotificationPreferences().getPreferredSMS());
    }
}
