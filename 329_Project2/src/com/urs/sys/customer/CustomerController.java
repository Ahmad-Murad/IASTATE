/**
 * 
 */
package com.urs.sys.customer;

import java.util.List;

import com.urs.db.DatabaseSupport;
import com.urs.db.Transaction;

/**
 * @author Andrew
 * 
 */
public class CustomerController
{
    public Customer getCustomer(long id) {
        return DatabaseSupport.getConnection().getCustomer(id);
    }

    public void updateCustomer(Customer c) {
        DatabaseSupport.getConnection().putCustomer(c);
    }

    public List<Transaction> getTransactionHistory(long customerID) {
        return DatabaseSupport.getConnection().getCustomer(customerID).getTransactionHistory();
    }

    public void updateNotificationSettings(Customer c) {

    }

    public void checkForNotifications() {

    }
}
