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
import com.urs.sys.payment.PaymentInfo;
import com.urs.test.whitebox.CustomerTest;
import com.urs.test.whitebox.DatabaseTest;
import com.urs.test.whitebox.InventoryTest;
import com.urs.test.whitebox.PaymentTest;
import com.urs.test.whitebox.UITest;

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

    public static CreditCard newCC(Customer c) {
        return new CreditCard(r.nextLong(), r.nextInt(), new Date(), c.getName());
    }

    public static Product newProd() {
        return new Product("Product" + r.nextInt(), r.nextBoolean());
    }

    public static Transaction newTrans(PaymentInfo info) {
        return new Transaction(info);
    }
}
