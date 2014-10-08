package com.urs.db;

import java.util.HashSet;
import java.util.Set;

public class Transaction {
    private final long transactionTimestamp = System.currentTimeMillis();
    private String customerName;
    private Set<Product> products = new HashSet<Product>();
    private double totalCost = 0.0;

    public Transaction(String customerName) {
        this.customerName = customerName;
    }

    public void addRental(Product arg) {
        if (products.add(arg))
            totalCost += arg.getRentalCost();
    }

    public String getCustomerName() {
        return customerName;
    }

    public String generateStatement() {
        StringBuffer sb = new StringBuffer("Statement record for " + getCustomerName() + "\n");

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
        sb.append("Sale record for " + getCustomerName() + "<br>\n");
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