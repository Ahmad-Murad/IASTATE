/**
 * 
 */
package se339.hw6.products.discounts;

import se339.hw6.products.Rentable;
import se339.hw6.products.Sellable;

/**
 * @author Andrew
 */
public class DefaultCostStrategy implements CostStrategy {

    public static double getDefaultRentalCost(Rentable prod) {
        return prod.getRentalCost();
    }

    public static double getDefaultSellCost(Sellable prod) {
        return prod.getSellCost();
    }

    @Override
    public double getRentalCost(Rentable product) {
        return DefaultCostStrategy.getDefaultRentalCost(product);
    }

    @Override
    public double getSellCost(Sellable product) {
        return DefaultCostStrategy.getDefaultSellCost(product);
    }
}
