/**
 * 
 */
package se339.hw6.products.discounts;

import se339.hw6.products.Rentable;
import se339.hw6.products.Sellable;

/**
 * @author Andrew
 */
public class Tier2DiscountStrategy implements CostStrategy
{
    @Override
    public double getRentalCost(Rentable product) {
        return 0.5 * DefaultCostStrategy.getDefaultRentalCost(product);
    }

    @Override
    public double getSellCost(Sellable product) {
        return 0.5 * DefaultCostStrategy.getDefaultSellCost(product);
    }
}
