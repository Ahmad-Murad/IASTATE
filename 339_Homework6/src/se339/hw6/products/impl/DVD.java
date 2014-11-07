/**
 * 
 */
package se339.hw6.products.impl;

import se339.hw6.products.AbstractProduct;


/**
 * @author Andrew
 */
public class DVD extends AbstractProduct
{

    public DVD(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public DVD(String name, boolean isNewRelease, int daysRented) {
        super(name, isNewRelease, daysRented, 5);
    }

    @Override
    public double getRentalCost() {
        return (3.00 * daysRented) + daysOverdue() > 0 ? 4.00 : 0.00;
    }

    @Override
    public double getSellCost() {
        return 20.00;
    }

}
