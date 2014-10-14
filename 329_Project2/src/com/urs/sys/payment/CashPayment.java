/**
 * 
 */
package com.urs.sys.payment;

/**
 * @author aguibert
 */
public class CashPayment implements PaymentInfo {

    private double cashAmount;

    public CashPayment(double dollarAmount) {
        this.cashAmount = dollarAmount;
    }

    @Override
    public double processPayment(double amount) {
        if (amount < 0.0 || amount > cashAmount)
            return cashAmount;

        return (cashAmount - amount);
    }
}
