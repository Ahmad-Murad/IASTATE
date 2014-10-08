/**
 * 
 */
package com.urs.sys.customer;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.urs.db.Product;
import com.urs.db.Transaction;
import com.urs.sys.payment.PaymentInfo;

/**
 * @author Andrew
 */
public class Customer implements Serializable
{
    private static final long serialVersionUID = -567768441247469162L;
    private final long customerID = System.currentTimeMillis();
    private String name;
    private String email;
    private String phone;
    private Set<PaymentInfo> paymentInfo = new HashSet<PaymentInfo>();
    private Map<Long, Transaction> transactionHistory = new HashMap<Long, Transaction>();
    private Map<Long, Product> outstandingRentals = new HashMap<Long, Product>();

    public long getID() {
        return this.customerID;
    }
}
