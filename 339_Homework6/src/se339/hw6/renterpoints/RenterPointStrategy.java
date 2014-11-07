/**
 * 
 */
package se339.hw6.renterpoints;

import java.util.Set;

import se339.hw6.products.Rentable;

/**
 * @author Andrew
 */
public interface RenterPointStrategy {
    public int getFrequentRenterPoints(Set<Rentable> rentals);
}
