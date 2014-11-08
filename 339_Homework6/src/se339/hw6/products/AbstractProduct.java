/**
 * 
 */
package se339.hw6.products;

/**
 * @author Andrew
 */
public abstract class AbstractProduct implements Rentable, Sellable
{
    protected final int daysRented;
    private final String name;
    private final boolean isNewRelease;
    private final boolean isRental;
    private final int OVERDUE_PERIOD;

    public AbstractProduct(String name, boolean isNewRelease) {
        this(name, isNewRelease, -1, -1);
    }

    public AbstractProduct(String name, boolean isNewRelease, int daysRented, int overduePeriod) {
        this.name = name;
        this.isNewRelease = isNewRelease;
        this.daysRented = daysRented;
        this.isRental = daysRented > 0;
        this.OVERDUE_PERIOD = overduePeriod;
    }

    @Override
    public final String getName() {
        return name;
    }

    @Override
    public abstract double getRentalCost();

    @Override
    public abstract double getSellCost();

    @Override
    public boolean isNewRelease() {
        return this.isNewRelease;
    }

    @Override
    public final int daysOverdue() {
        verifyProductRentable();
        return (daysRented > OVERDUE_PERIOD) ? (daysRented - OVERDUE_PERIOD) : 0;
    }

    @Override
    public final int getDaysRented() {
        verifyProductRentable();
        return this.daysRented;
    }

    private double getCost() {
        return isRental ? getRentalCost() : getSellCost();
    }

    @Override
    public String toString() {
        String ret = getName() + "  \tNew_Release=" + (isNewRelease() ? "Y" : "N") + "\tCost=" + getCost();
        if (isRental)
            ret += "\tDaysOverdue=" + daysOverdue();
        return ret;
    }

    public void verifyProductRentable() {
        if (!isRental)
            throw new NonRentalException("Product was not rented!");
    }

    public void verifyProductSellable() {
        if (isRental)
            throw new NonSellableException("Product was not rented!");
    }

    public boolean isRental() {
        return isRental;
    }
}
