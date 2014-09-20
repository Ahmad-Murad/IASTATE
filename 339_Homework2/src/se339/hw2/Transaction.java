package se339.hw2;

import java.util.HashSet;
import java.util.Set;

public class Transaction {
    private String customerName;
    private Set<Rentable> rentals = new HashSet<Rentable>();
    private double totalRentalCost = 0.0;
    private int frequentRenterPoints = 0;

    public Transaction(String customerName) {
        this.customerName = customerName;
    }

    public void addRental(Rentable arg) {
        if (rentals.add(arg)) {
            totalRentalCost += arg.getCost();
            frequentRenterPoints += arg.getFrequentRenterPoints();
        }
    }

    public String getCustomerName() {
        return customerName;
    }

    public String generateStatement() {
        StringBuffer sb = new StringBuffer("Rental record for " + getCustomerName() + "\n");
        double totalAmount = 0.0;
        int totalPoints = 0;

        for (Rentable rental : rentals) {
            sb.append(rental).append('\n');
        }

        // add footer lines
        sb.append("Amount owed is: ").append(totalAmount).append('\n');
        sb.append("You earned ").append(totalPoints).append(" frequent renter points");
        return sb.toString();
    }

    public String generateStatementHTML() {
        StringBuffer sb = new StringBuffer("<html>\n");
        sb.append("<head>\n");
        sb.append("Rental record for " + getCustomerName() + "<br>\n");
        sb.append("</head>\n<body>\n");
        for (Rentable rental : rentals)
            sb.append("  <li> " + rental.toString() + " <br>\n");
        sb.append("Amount owed is: ").append(totalRentalCost).append("<br>\n");
        sb.append("You earned ").append(frequentRenterPoints).append(" frequent renter points<br>\n");
        sb.append("</body>\n</head>\n");
        return sb.toString();
    }
}