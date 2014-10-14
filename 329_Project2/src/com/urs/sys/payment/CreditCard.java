/**
 * 
 */
package com.urs.sys.payment;

import java.util.Date;

/**
 * @author aguibert
 */
public class CreditCard implements PaymentInfo
{
    private final Date expirationDate;
    private final long cardNumber;
    private final short CVVCode;
    private final String cardHolderName;
    private double balance = 1000.0; // TODO for testing, everyone starts with $1000

    public CreditCard(long cardNumber, short CVVCode, Date exp, String custName) {
        this.cardHolderName = custName;
        this.expirationDate = exp;
        this.CVVCode = CVVCode;
        this.cardNumber = cardNumber;
    }

    @Override
    public double processPayment(double amount) throws Exception {
        postTransaction(amount);
        return 0.0; // no cash back for credit card transactions
    }

    /**
     * Post transaction with credit card to a bank.
     * 
     * @param amount Amount of the transaction.
     * @return True if transaction successful, false otherwise.
     */
    public boolean postTransaction(double amount) {
        if (amount < 0.0)
            return false;

        // TODO this should post a transaction to the bank to have the amount validated 
        if (balance > amount)
            return true;

        return false;
    }
}
