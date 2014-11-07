/**
 * 
 */
package se339.hw6.products.impl;

import se339.hw6.products.AbstractProduct;


/**
 * @author Andrew
 */
public class VideoGame extends AbstractProduct
{

    public VideoGame(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public VideoGame(String name, boolean isNewRelease, int daysRented) {
        super(name, isNewRelease, daysRented, 30);
    }

    @Override
    public double getRentalCost() {
        return (2.00 * daysRented) + (3.00 * daysOverdue());
    }

    @Override
    public double getSellCost() {
        return 60.00;
    }

}
