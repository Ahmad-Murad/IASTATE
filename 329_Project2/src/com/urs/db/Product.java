/**
 * 
 */
package com.urs.db;

import com.urs.sys.customer.Customer;

/**
 * @author Andrew
 */
public abstract class Product
{
    private final long productTimestamp = System.currentTimeMillis();
    private final String name;
    private boolean isNewRelease;
    protected final int daysRented;
    private final boolean isRental;
    private Customer currentHolder;

    public Product(String name, boolean isNewRelease) {
        this(name, isNewRelease, -1);
    }

    public Product(String name, boolean isNewRelease, int daysRented) {
        this.name = name;
        this.isNewRelease = isNewRelease;
        this.daysRented = daysRented;
        this.isRental = daysRented > 0;
    }

    public final String getName() {
        return name;
    }

    public abstract double getRentalCost();

    public boolean isNewRelease() {
        return this.isNewRelease;
    }

    public abstract int daysOverdue();

    public int getDaysRented() {
        return this.daysRented;
    }

    @Override
    public String toString() {
        String ret = getName() + "  \tNew_Release=" + (isNewRelease() ? "Y" : "N") + "\tCost=" + getRentalCost();
        if (isRental)
            ret += "\tDaysOverdue=" + daysOverdue();
        return ret;
    }

    public long getID() {
        return this.productTimestamp;
    }
}
