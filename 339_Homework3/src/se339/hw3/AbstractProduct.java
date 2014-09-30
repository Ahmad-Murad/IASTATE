/**
 * 
 */
package se339.hw3;

/**
 * @author Andrew
 */
public abstract class AbstractProduct implements Rentable, Sellable
{
    private final String name;
    private boolean isNewRelease;
    protected final int daysRented;
    private final boolean isRental;

    public AbstractProduct(String name, boolean isNewRelease) {
        this(name, isNewRelease, -1);
    }

    public AbstractProduct(String name, boolean isNewRelease, int daysRented) {
        this.name = name;
        this.isNewRelease = isNewRelease;
        this.daysRented = daysRented;
        this.isRental = daysRented > 0;
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
    public abstract int daysOverdue();

    @Override
    public int getDaysRented() {
        verifyProductRentable();
        return this.daysRented;
    }

    @Override
    public int getFrequentRenterPoints() {
        return (isNewRelease && daysRented > 1) ? 2 : 1;
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

    public final void verifyProductRentable() {
        if (!isRental)
            throw new NonRentalException("Product was not rented!");
    }

    public final void verifyProductSellable() {
        if (isRental)
            throw new NonSellableException("Product was not rented!");
    }
}
