/**
 * 
 */
package se339.hw6.products.renterpoints;

import se339.hw6.products.Rentable;


/**
 * @author Andrew
 */
public interface RenterPointStrategy {
    public int getFrequentRenterPoints(Rentable rental);
}
