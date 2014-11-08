/**
 * 
 */
package se339.hw6.products.impl;

/**
 * @author aguibert
 */
public class ChildrensMovie extends AbstractMovie
{
    public ChildrensMovie(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public ChildrensMovie(String name, int daysRented, boolean isNewRelease) {
        super(name, isNewRelease, daysRented, 2);
    }

    @Override
    public double getRentalCost() {
        verifyProductRentable();
        if (isNewRelease())
            return 3.0 * getDaysRented();
        else
            return 1.5 + (daysOverdue() * 1.5);
    }

    @Override
    public double getSellCost() {
        verifyProductSellable();
        return 3.50;
    }
}
