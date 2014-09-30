package se339.hw3;

import java.util.HashSet;
import java.util.Set;

import se339.hw3.products.Rentable;
import se339.hw3.products.Sellable;

public class Transaction {
    private String customerName;
    private Set<Rentable> rentals = new HashSet<Rentable>();
    private Set<Sellable> sales = new HashSet<Sellable>();
    private double totalCost = 0.0;
    private int frequentRenterPoints = 0;

    public Transaction(String customerName) {
        this.customerName = customerName;
    }

    public void addRental(Rentable arg) {
        if (rentals.add(arg)) {
            totalCost += arg.getRentalCost();
            frequentRenterPoints += arg.getFrequentRenterPoints();
        }
    }

    public void addSellable(Sellable arg) {
        if (sales.add(arg))
            totalCost += arg.getSellCost();
    }

    public String getCustomerName() {
        return customerName;
    }

    public String generateStatement() {
        StringBuffer sb = new StringBuffer("Statement record for " + getCustomerName() + "\n");

        if (rentals.size() != 0) {
            sb.append("Products rented:\n");
            for (Rentable rental : rentals)
                sb.append("  ").append(rental).append('\n');
        }
        if (sales.size() != 0) {
            sb.append("Products purchased:\n");
            for (Sellable sale : sales)
                sb.append("  ").append(sale).append('\n');
        }

        // add footer lines
        sb.append("Amount owed is: ").append(totalCost).append('\n');
        if (rentals.size() != 0)
            sb.append("You earned ").append(frequentRenterPoints).append(" frequent renter points");
        return sb.toString();

    }

    public String generateStatementHTML() {
        StringBuffer sb = new StringBuffer("<html>\n");
        sb.append("<head>\n");
        sb.append("Sale record for " + getCustomerName() + "<br>\n");
        sb.append("</head>\n<body>\n");
        sb.append(generateStatement().replace("\n", "<br>\n"));
        sb.append("</body>\n</head>\n");
        return sb.toString();
    }
}