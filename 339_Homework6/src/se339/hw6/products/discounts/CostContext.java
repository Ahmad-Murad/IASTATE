/**
 * 
 */
package se339.hw6.products.discounts;

import se339.hw6.products.Rentable;
import se339.hw6.products.Sellable;

/**
 * @author Andrew
 */
public class CostContext
{
    private CostStrategy strategy = new DefaultCostStrategy();

    public void setCostStrategy(CostStrategy strat) {
        strategy = strat;
    }

    public double getSellCost(Sellable product) {
        return strategy.getSellCost(product);
    }

    public double getRentalCost(Rentable r) {
        return strategy.getRentalCost(r);
    }
}
