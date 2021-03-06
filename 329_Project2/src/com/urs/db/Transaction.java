package com.urs.db;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import com.urs.sys.customer.Customer;
import com.urs.sys.payment.PaymentInfo;

public class Transaction {
    private final long transactionTimestamp = new Random().nextLong();
    private String customerName;
    private PaymentInfo paymentInfo;
    private Set<Product> products = new HashSet<Product>();
    private double totalCost = 0.0;

    public Transaction(PaymentInfo info) {
        if (info instanceof Customer)
            customerName = ((Customer) info).getName();
        this.paymentInfo = info;
    }

    public void addProduct(Product arg) {
        if (products.add(arg))
            totalCost += arg.getRentalCost();
    }

    public void complete() {
        double change = paymentInfo.processPayment(totalCost);
        if (change > 0.0)
            System.out.println("Returning change: " + change);
    }

    public double getTotal() {
        return totalCost;
    }

    public String generateStatement() {
        StringBuffer sb = new StringBuffer("Statement record " + (customerName == null ? ("for customer " + customerName) : String.valueOf(transactionTimestamp)) + "\n");

        if (products.size() != 0) {
            sb.append("Products rented:\n");
            for (Product rental : products)
                sb.append("  ").append(rental).append('\n');
        }

        // add footer lines
        sb.append("Amount owed is: ").append(totalCost).append('\n');
        return sb.toString();
    }

    public String generateStatementHTML() {
        StringBuffer sb = new StringBuffer("<html>\n");
        sb.append("<head>\n");
        sb.append("Sale record for customer " + (customerName == null ? ("for customer " + customerName) : String.valueOf(transactionTimestamp)) + "<br>\n");
        sb.append("</head>\n<body>\n");
        for (Product p : products)
            sb.append("  <li> " + p.toString() + " <br>\n");
        sb.append("Amount owed is: ").append(totalCost).append("<br>\n");
        sb.append("</body>\n</head>\n");
        return sb.toString();
    }

    public long getID() {
        return this.transactionTimestamp;
    }
}