/**
 * 
 */
package se339.hw3.products.impl;

import se339.hw3.products.AbstractProduct;


/**
 * @author Andrew
 */
public class Book extends AbstractProduct
{
    public Book(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public Book(String name, boolean isNewRelease, int daysRented) {
        super(name, isNewRelease, daysRented, 60);
    }

    @Override
    public double getRentalCost() {
        return daysRented * 0.25 + daysOverdue() > 0 ? 5.00 : 0.00;
    }

    @Override
    public double getSellCost() {
        return 30.00;
    }

}
