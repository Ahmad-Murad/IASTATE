/**
 * 
 */
package com.urs.sys.customer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.urs.db.Product;
import com.urs.db.Transaction;
import com.urs.sys.payment.CreditCard;
import com.urs.sys.payment.PaymentInfo;

/**
 * @author Andrew
 */
public class Customer implements Serializable, PaymentInfo
{
    private static final long serialVersionUID = -567768441247469162L;
    private final long customerID = new Random().nextLong();
    private String name;
    private String email;
    private String phone;
    private Set<CreditCard> creditCards = new HashSet<>();
    private CreditCard preferredCard;
    private Map<Long, Transaction> transactionHistory = new HashMap<>();
    private Map<Long, Product> outstandingRentals = new HashMap<>();

    public List<Transaction> getTransactionHistory() {
        List<Long> keys = new ArrayList<>(transactionHistory.keySet());
        Collections.sort(keys);
        List<Transaction> toReturn = new ArrayList<>();
        for (long key : keys) {
            toReturn.add(transactionHistory.get(key));
        }
        return toReturn;
    }

    public long getID() {
        return this.customerID;
    }

    public void addCreditCard(CreditCard newCard) {
        creditCards.add(newCard);
        if (creditCards.size() == 1)
            preferredCard = newCard;
    }

    @Override
    public double processPayment(double amount) {

    }
}
