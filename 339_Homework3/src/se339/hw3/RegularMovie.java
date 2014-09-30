/**
 * 
 */
package se339.hw3;

/**
 * @author aguibert
 */
public class RegularMovie extends AbstractProduct
{
    private static final int OVERDUE_PERIOD = 2;

    public RegularMovie(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public RegularMovie(String name, int daysRented, boolean isNewRelease) {
        super(name, isNewRelease, daysRented);
    }

    @Override
    public int daysOverdue() {
        return (daysRented > OVERDUE_PERIOD) ? (daysRented - OVERDUE_PERIOD) : 0;
    }

    @Override
    public double getRentalCost() {
        verifyProductRentable();
        if (isNewRelease())
            return 3.0 * getDaysRented();
        else
            return 2.0 + (daysOverdue() * 1.5);
    }

    @Override
    public double getSellCost() {
        verifyProductSellable();
        return 3.50;
    }
}
