/**
 * 
 */
package se339.hw6.products.renterpoints;

import se339.hw6.products.Rentable;

/**
 * @author Andrew
 */
public class YoungCustomerRenterPointStrategy implements RenterPointStrategy
{
    @Override
    public int getFrequentRenterPoints(Rentable rental) {
        return 2 * DefaultRenterPointStrategy.getDefaultFrequentRenterPoints(rental);
    }
}
