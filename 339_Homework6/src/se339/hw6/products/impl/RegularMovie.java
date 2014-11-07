/**
 * 
 */
package se339.hw6.products.impl;

import se339.hw6.products.AbstractProduct;


/**
 * @author aguibert
 */
public class RegularMovie extends AbstractProduct
{
    public RegularMovie(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public RegularMovie(String name, int daysRented, boolean isNewRelease) {
        super(name, isNewRelease, daysRented, 2);
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
