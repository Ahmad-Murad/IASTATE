/**
 * 
 */
package se339.hw3.products.impl;

import se339.hw3.products.AbstractProduct;


/**
 * @author Andrew
 */
public class BluerayMovie extends AbstractProduct
{
    public BluerayMovie(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public BluerayMovie(String name, boolean isNewRelease, int daysRented) {
        super(name, isNewRelease, daysRented, 4);
    }

    @Override
    public double getRentalCost() {
        return (5.00 * daysRented) + (2.00 * daysOverdue());
    }

    @Override
    public double getSellCost() {
        return 25.00;
    }
}
