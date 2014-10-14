/**
 * 
 */
package com.urs.test;

import java.util.Date;
import java.util.Random;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.urs.db.Product;
import com.urs.db.Transaction;
import com.urs.sys.customer.Customer;
import com.urs.sys.payment.CreditCard;

/**
 * @author Andrew
 * 
 */
@RunWith(Suite.class)
@SuiteClasses({
               CustomerTest.class,
               DatabaseTest.class,
               InventoryTest.class,
               PaymentTest.class,
               UITest.class
})
public class _UnifiedRentalServiceTest {
    static Random r = new Random();

    public static Customer newCust() {
        return new Customer("Customer" + r.nextInt(), "Email" + r.nextInt() + "@email.com", String.valueOf(r.nextInt()));
    }

    public static CreditCard newCC(String cName) {
        return new CreditCard(r.nextLong(), r.nextInt(), new Date(), cName);
    }

    public static Product newProd() {
        return new Product("Product" + r.nextInt(), r.nextBoolean());
    }

    public static Transaction newTrans(long cid) {
        return new Transaction(cid);
    }
}
