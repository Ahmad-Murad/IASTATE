/**
 * 
 */
package se339.hw6.products.renterpoints;

import se339.hw6.products.Rentable;

/**
 * @author Andrew
 */
public class RenterPointContext
{
    private RenterPointStrategy strategy;

    public void setRenterPointStrategy(RenterPointStrategy strat) {
        strategy = strat;
    }

    public int getRenterPoints(Rentable r) {
        return strategy.getFrequentRenterPoints(r);
    }
}
