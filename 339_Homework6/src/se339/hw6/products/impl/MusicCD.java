/**
 * 
 */
package se339.hw6.products.impl;

import se339.hw6.products.AbstractProduct;


/**
 * @author Andrew
 */
public class MusicCD extends AbstractProduct
{
    public MusicCD(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public MusicCD(String name, boolean isNewRelease, int daysRented) {
        super(name, isNewRelease, daysRented, 14);
    }

    @Override
    public double getRentalCost() {
        return (1.00 * daysRented) + (2.00 * daysOverdue());
    }

    @Override
    public double getSellCost() {
        return 15.00;
    }

}
