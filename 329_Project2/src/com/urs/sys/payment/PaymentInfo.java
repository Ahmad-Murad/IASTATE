/**
 * 
 */
package com.urs.sys.payment;

/**
 * @author Andrew
 */
public interface PaymentInfo
{
    // This class should be encrypted when stored in the database
    public double processPayment(double amount);
}
