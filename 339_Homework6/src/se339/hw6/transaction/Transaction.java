package se339.hw6.transaction;

import java.util.HashSet;
import java.util.Set;

import se339.hw6.products.Rentable;
import se339.hw6.products.Sellable;
import se339.hw6.products.discounts.CostContext;
import se339.hw6.products.discounts.DefaultCostStrategy;
import se339.hw6.products.discounts.Tier1DiscountStrategy;
import se339.hw6.products.discounts.Tier2DiscountStrategy;
import se339.hw6.products.impl.AbstractMovie;
import se339.hw6.products.renterpoints.DefaultRenterPointStrategy;
import se339.hw6.products.renterpoints.MultiTypeRenterPointStrategy;
import se339.hw6.products.renterpoints.RenterPointContext;
import se339.hw6.products.renterpoints.YoungCustomerRenterPointStrategy;

public class Transaction {
    private Customer customer;
    private Set<Rentable> rentals = new HashSet<Rentable>();
    private Set<Sellable> sales = new HashSet<Sellable>();
    private double totalCost = 0.0;
    private RenterPointContext renterPointCtx = new RenterPointContext();
    private CostContext costCtx = new CostContext();

    public Transaction(Customer c) {
        this.customer = c;
    }

    public void addRental(Rentable newRental) {
        rentals.add(newRental);

        // Products after 5 rentals are 50% off
        if (rentals.size() > 5)
            costCtx.setCostStrategy(new Tier2DiscountStrategy());
        // Products for 3-5 rentals are 20% off
        else if (rentals.size() >= 3)
            costCtx.setCostStrategy(new Tier1DiscountStrategy());
        else
            costCtx.setCostStrategy(new DefaultCostStrategy());

        totalCost += costCtx.getRentalCost(newRental);
    }

    public void addSellable(Sellable newSellable) {
        sales.add(newSellable);
        totalCost += costCtx.getSellCost(newSellable);
    }

    public String getCustomerName() {
        return customer.name;
    }

    public double getTotalCost() {
        return totalCost;
    }

    public int getTotalRenterPoints() {
        // Determine which strategy to use
        if (getNumRentalCategories() > 2)
            renterPointCtx.setRenterPointStrategy(new MultiTypeRenterPointStrategy());
        else if (customer.age >= 18 && customer.age <= 22 && getNumNewReleasesMovies() > 0)
            renterPointCtx.setRenterPointStrategy(new YoungCustomerRenterPointStrategy());
        else
            renterPointCtx.setRenterPointStrategy(new DefaultRenterPointStrategy());

        // Calculate renter points using strategy
        int totalPoints = 0;
        for (Rentable r : rentals)
            totalPoints += renterPointCtx.getRenterPoints(r);
        return totalPoints;
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
            sb.append("You earned ").append(getTotalRenterPoints()).append(" frequent renter points");
        return sb.toString();

    }

    public String generateStatementHTML() {
        StringBuffer sb = new StringBuffer("<html>\n");
        sb.append("<head>\n");
        sb.append("<h1>Sale record for " + getCustomerName() + "</h1>\n");
        sb.append("</head>\n<body>\n");
        sb.append(generateStatement().replace("\n", "<br>\n"));
        sb.append("\n</body>\n</head>\n</html>");
        return sb.toString();
    }

    private int getNumRentalCategories() {
        Set<Class<?>> categoryTypes = new HashSet<Class<?>>();

        for (Rentable r : rentals)
            categoryTypes.add(r.getClass());

        return categoryTypes.size();
    }

    private int getNumNewReleasesMovies() {
        int newReleaseMovies = 0;
        for (Rentable r : rentals)
            if ((r instanceof AbstractMovie) && r.isNewRelease())
                newReleaseMovies++;
        return newReleaseMovies;
    }
}