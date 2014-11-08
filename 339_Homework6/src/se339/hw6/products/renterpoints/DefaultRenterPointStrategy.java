/**
 * 
 */
package se339.hw6.products.renterpoints;

import se339.hw6.products.Rentable;

/**
 * @author Andrew
 */
public class DefaultRenterPointStrategy implements RenterPointStrategy
{
    public static int getDefaultFrequentRenterPoints(Rentable rental) {
        return (rental.isNewRelease() && rental.getDaysRented() > 1) ? 2 : 1;
    }

    @Override
    public int getFrequentRenterPoints(Rentable rental) {
        return DefaultRenterPointStrategy.getDefaultFrequentRenterPoints(rental);
    }
}
