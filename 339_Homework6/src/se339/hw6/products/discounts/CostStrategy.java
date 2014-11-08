/**
 * 
 */
package se339.hw6.products.discounts;

import se339.hw6.products.Rentable;
import se339.hw6.products.Sellable;

/**
 * @author Andrew
 */
public interface CostStrategy {
    public double getRentalCost(Rentable product);

    public double getSellCost(Sellable product);
}
