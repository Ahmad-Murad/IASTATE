/**
 * 
 */
package se339.hw6.products.impl;

import se339.hw6.products.AbstractProduct;

/**
 * @author Andrew
 * 
 */
public abstract class AbstractMovie extends AbstractProduct
{
    public AbstractMovie(String name, boolean isNewRelease) {
        super(name, isNewRelease);
    }

    public AbstractMovie(String name, boolean isNewRelease, int daysRented, int overduePeriod) {
        super(name, isNewRelease, daysRented, overduePeriod);
    }
}
