package cs417.hw3;

import java.util.Scanner;

public class Account
{
    private double balance;
    private String name;
    private long acctNum;
    public static int numAccounts = 0;

    //----------------------------------------------
    //Constructor -- initializes balance, owner, and account number
    //----------------------------------------------
    public Account(double initBal, String owner, long number) throws Exception
    {
        if (initBal <= 0)
            throw new Exception("Invalid amount");
        balance = initBal;
        name = owner;
        acctNum = number;
        numAccounts++;
    }

    //----------------------------------------------
    // Checks to see if balance is sufficient for withdrawal.
    // If so, decrements balance by amount; if not, prints message.
    //----------------------------------------------
    public void withdraw(double amount) throws Exception
    {
        if (amount <= 0)
            throw new Exception("Invalid amount");
        if (balance >= amount)
            balance -= amount;
        else
            throw new Exception("Insufficient Fund");
    }

    //----------------------------------------------
    // Adds deposit amount to balance.
    //----------------------------------------------
    public void deposit(double amount) throws Exception
    {
        if (amount <= 0)
            throw new Exception("Invalid amount");
        balance += amount;
    }

    //----------------------------------------------
    // Returns balance.
    //----------------------------------------------
    public double getBalance()
    {
        return balance;
    }

    //----------------------------------------------
    // Returns account number.
    //----------------------------------------------
    public long getAcctNumber()
    {
        return acctNum;
    }

    //----------------------------------------------
    // Changes account holder name.
    //----------------------------------------------
    public void ChangeAccountName(String newName)
    {
        this.name = newName;
    }

    //----------------------------------------------
    // Changes account number.
    //----------------------------------------------
    public void ChangeAccountNumber(long newAcctNumber)
    {
        this.acctNum = newAcctNumber;
    }

    //----------------------------------------------
    // Returns a string containing the name, account number, and balance.
    //----------------------------------------------
    @Override
    public String toString()
    {
        return "Name: " + name +
               "\nAccount Number: " + acctNum +
               "\nBalance: " + balance;
    }

    public static int getNumAccounts()
    {
        return numAccounts;
    }

    public void close()
    {
        name = name + "CLOSED";
        balance = 0;
        numAccounts--;
    }

    public static Account Accountconsolidate(Account acct1, Account acct2) throws Exception
    {
        double newBalance;
        if (acct1.acctNum == acct2.acctNum)
        {
            System.out.println("Same account numbers cannot be consolidated.");
            return null;
        }
        if (acct1.name != acct2.name)
        {
            System.out.println("Different account names cannot be consolidated.");
            return null;
        }
        else
        {
            newBalance = acct1.balance + acct2.balance;
            acct1.close();
            acct2.close();
        }
        Account newAccount = new Account(newBalance, acct1.name, 27);
        return newAccount;

    }

    public static void main(String[] args) throws Exception
    {
        Account testAcct;

        Scanner scan = new Scanner(System.in);

        System.out.println("How many accounts would you like to create?");
        int num = scan.nextInt();

        for (int i = 1; i <= num; i++)
        {
            testAcct = new Account(100, "Name-" + i, i);
            System.out.println("\nCreated account " + testAcct);
            System.out.println("Now there are " + Account.getNumAccounts() +
                               " accounts");
        }
        scan.close();
    }

}