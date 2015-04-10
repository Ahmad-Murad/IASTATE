package cs417.hw3.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import org.junit.Assert;
import org.junit.Test;

import cs417.hw3.Account;

/**
 * @author Andrew
 */
public class AccountTestAfter {
    @Test(expected = Exception.class)
    public void testZeroBalance() throws Exception {
        new Account(0.0, "Andy", 123);
    }

    @Test(expected = Exception.class)
    public void testNegativeBalance() throws Exception {
        new Account(-1.0, "Andy", 123);
    }

    @Test
    public void testMinBalance() throws Exception {
        new Account(0.01, "Andy", 123);
    }

    @Test(expected = Exception.class)
    public void testOverWithdraw() throws Exception {
        Account a = new Account(100, "Andy", 123);
        a.withdraw(99);
        a.withdraw(2);
    }

    @Test
    public void testWithdrawAll() throws Exception {
        Account a = new Account(100, "Andy", 123);
        a.withdraw(100);
    }

    @Test(expected = Exception.class)
    public void testWithdrawNegative() throws Exception {
        Account a = new Account(100, "Andy", 123);
        a.withdraw(-50);
    }

    @Test(expected = Exception.class)
    public void testWithdrawZero() throws Exception {
        Account a = new Account(100, "Andy", 123);
        a.withdraw(0.0);
    }

    @Test(expected = Exception.class)
    public void testDepositNegative() throws Exception {
        Account a = new Account(100, "Andy", 123);
        a.deposit(-50);
    }

    @Test(expected = Exception.class)
    public void testDepositZero() throws Exception {
        Account a = new Account(100, "Andy", 123);
        a.deposit(0.0);
    }

    @Test
    public void testDeposit() throws Exception {
        Account a = new Account(100, "Andy", 123);
        a.deposit(100.0);
        assertEquals(200.0, a.getBalance(), 0.00001);
    }

    @Test
    public void testGetBalance() throws Exception {
        Account a = new Account(100, "Andy", 123);
        Assert.assertEquals(100.0, a.getBalance(), 0.0001);
    }

    @Test
    public void testChangeAccountName() throws Exception {
        Account a = new Account(100, "Andy", 123);
        assertEquals("Name: Andy\nAccount Number: 123\nBalance: 100.0", a.toString());
        a.ChangeAccountName("Andrew");
        assertEquals("Name: Andrew\nAccount Number: 123\nBalance: 100.0", a.toString());
    }

    @Test
    public void testChangeAccountNumber() throws Exception {
        Account a = new Account(100, "Andy", 123);
        assertEquals("Name: Andy\nAccount Number: 123\nBalance: 100.0", a.toString());
        a.ChangeAccountNumber(321);
        assertEquals(321, a.getAcctNumber());
        assertEquals("Name: Andy\nAccount Number: 321\nBalance: 100.0", a.toString());
    }

    @Test
    public void testGetNumAccounts() throws Exception {
        Account.numAccounts = 0; // reset number of accounts
        new Account(100, "Andy1", 123);
        Assert.assertEquals(1, Account.getNumAccounts());
        new Account(100, "Andy2", 123);
        Assert.assertEquals(2, Account.getNumAccounts());
        new Account(100, "Andy3", 123);
        Assert.assertEquals(3, Account.getNumAccounts());
    }

    @Test
    public void testCloseName() throws Exception {
        Account a = new Account(100, "Andy", 123);
        assertEquals("Name: Andy\nAccount Number: 123\nBalance: 100.0", a.toString());
        a.close();
        assertEquals("Name: AndyCLOSED\nAccount Number: 123\nBalance: 0.0", a.toString());
        // it seems wrong that an account can be closed multiple times...
        a.close();
        assertEquals("Name: AndyCLOSEDCLOSED\nAccount Number: 123\nBalance: 0.0", a.toString());
    }

    @Test
    public void testCloseBalance() throws Exception {
        Account a = new Account(100, "Andy", 123);
        assertEquals(100.0, a.getBalance(), 0.00001);
        a.close();
        assertEquals(0.0, a.getBalance(), 0.00001);
    }

    @Test
    public void testCloseNumAccounts() throws Exception {
        Account.numAccounts = 0; // reset number of accounts
        Account a1 = new Account(100, "Andy1", 123);
        Account a2 = new Account(100, "Andy2", 123);
        assertEquals(Account.numAccounts, 2);
        a1.close();
        assertEquals(Account.numAccounts, 1);
        a2.close();
        assertEquals(Account.numAccounts, 0);
        a2.close();
        assertEquals(Account.numAccounts, -1);
    }

    @Test
    public void testMergeAccountsByNumber() throws Exception {
        Account a1 = new Account(100, "Andy1", 123);
        Account a2 = new Account(100, "Andy2", 123);

        // Set a readable output stream
        PrintStream realOutStream = System.out;
        OutputStream out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));

        assertNull(Account.Accountconsolidate(a1, a2));

        // restore default output
        System.setOut(realOutStream);
        assertEquals("Same account numbers cannot be consolidated.".trim(), out.toString().trim());
    }

    @Test
    public void testMergeAccountsByName() throws Exception {
        Account a1 = new Account(100, "Andy1", 123);
        Account a2 = new Account(100, "Andy2", 124);

        // Set a readable output stream
        PrintStream realOutStream = System.out;
        OutputStream out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));

        assertNull(Account.Accountconsolidate(a1, a2));

        // restore default output
        System.setOut(realOutStream);
        assertEquals("Different account names cannot be consolidated.".trim(), out.toString().trim());
    }

    @Test
    public void testMergeAccounts() throws Exception {
        Account.numAccounts = 0; // reset number of accounts
        Account a1 = new Account(100, "Andy", 123);
        Account a2 = new Account(100, "Andy", 124);
        Account merged = Account.Accountconsolidate(a1, a2);

        // verify data from the closed accounts
        assertEquals(0.0, a1.getBalance(), 0.00001);
        assertEquals(0.0, a2.getBalance(), 0.00001);
        assertEquals("Name: AndyCLOSED\nAccount Number: 123\nBalance: 0.0", a1.toString());
        assertEquals("Name: AndyCLOSED\nAccount Number: 124\nBalance: 0.0", a2.toString());

        // verify data from the resulting merged account
        assertEquals(200.0, merged.getBalance(), 0.00001);
        assertEquals(27, merged.getAcctNumber());
        assertEquals("Name: AndyCLOSED\nAccount Number: 27\nBalance: 200.0", merged.toString());
        assertEquals(1, Account.numAccounts);
    }
}
