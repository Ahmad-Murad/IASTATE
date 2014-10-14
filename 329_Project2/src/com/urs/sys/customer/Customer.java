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
    private NotificationPreferences notificationPrefs = new NotificationPreferences();

    public Customer(String name, String email, String phone) {
        this.name = name;
        this.email = email;
        this.phone = phone;
    }

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

    public String getName() {
        return this.name;
    }

    public void addCreditCard(CreditCard newCard) {
        creditCards.add(newCard);
        preferredCard = newCard;
    }

    public String showCreditCards() {
        StringBuffer sb = new StringBuffer("Cards for " + name + ":\n");
        for (CreditCard card : creditCards)
            sb.append(' ' + card.toString() + '\n');
        return sb.toString();
    }

    public Transaction createTransaction() {
        Transaction t = new Transaction(this.getID());
        transactionHistory.put(t.getID(), t);
        return t;
    }

    @Override
    public double processPayment(double amount) {
        return preferredCard.processPayment(amount);
    }

    public NotificationPreferences getNotificationPreferences() {
        return this.notificationPrefs;
    }

    public class NotificationPreferences {
        private String sendEmail = null;
        private String sendSMS = null;

        public NotificationPreferences() {}

        public void disableAllNotifications() {
            this.sendEmail = null;
            this.sendSMS = null;
        }

        public void setPreferredEmail(String newEmail) {
            this.sendEmail = newEmail;
        }

        public void setPreferredSMS(String newSms) {
            this.sendSMS = newSms;
        }

        public String getPreferredEmail() {
            return this.sendEmail;
        }

        public String getPreferredSMS() {
            return this.sendSMS;
        }
    }
}
