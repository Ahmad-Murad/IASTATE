package com.urs.db;

import java.util.HashMap;
import java.util.Map;

import com.urs.sys.customer.Customer;

public class DatabaseSupport
{
    private Map<Long, Customer> customers = new HashMap<Long, Customer>();
    private Map<Long, Transaction> transactions = new HashMap<Long, Transaction>();
    private Map<Long, Product> products = new HashMap<Long, Product>();

    private static DatabaseSupport _instance = new DatabaseSupport();

    private DatabaseSupport() {} // Private constructor

    public static DatabaseSupport getConnection() {
        return _instance; // returns a singleton
    }

    public Customer getCustomer(long id) {
        return customers.get(id);
    }

    public void putCustomer(Customer c) {
        customers.put(c.getID(), c);
    }

    public Transaction getTransaction(long id) {
        return transactions.get(id);
    }

    public void putTransaction(Transaction t) {
        transactions.put(t.getID(), t);
    }

    public Product getProduct(long id) {
        return products.get(id);
    }

    public void putProduct(Product p) {
        products.put(p.getID(), p);
    }

    public void remove(Object o) {
        if (o instanceof Product)
            products.remove(((Product) o).getID());
        else if (o instanceof Customer)
            customers.remove(((Customer) o).getID());
        else if (o instanceof Transaction)
            transactions.remove(((Transaction) o).getID());
    }
}
