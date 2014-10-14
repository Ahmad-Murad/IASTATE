/**
 * 
 */
package com.urs.db;

import java.util.Random;

/**
 * @author Andrew
 */
public class Product
{
    private final long productTimestamp;
    private final String name;
    private boolean isNewRelease;
    protected final int daysRented;
    private final boolean isRental;
    private boolean isAvailable; // to be rented
    /** The ID of the customer who currently is renting this product */
    private long currentCustomerHolding;

    public Product(String name, boolean isNewRelease) {
        this(name, isNewRelease, -1);
    }

    public Product(String name, boolean isNewRelease, int daysRented) {
        this.productTimestamp = new Random().nextLong();
        this.name = name;
        this.isNewRelease = isNewRelease;
        this.daysRented = daysRented;
        this.isRental = daysRented > 0;
    }

    public final String getName() {
        return name;
    }

    public double getRentalCost() {
        return this.daysRented + (isNewRelease ? 1.5 : 1.0) + daysOverdue();
    }

    public double getSellCost() {
        return isNewRelease ? 25.0 : 15.0;
    }

    public boolean isNewRelease() {
        return this.isNewRelease;
    }

    public int daysOverdue() {
        return (this.daysRented > 3) ? (this.daysRented - 3) : 0;
    }

    public int getDaysRented() {
        return this.daysRented;
    }

    public void markRented(long customerID) {
        this.currentCustomerHolding = customerID;
        this.isAvailable = false;
    }

    public void returnProduct() {
        this.currentCustomerHolding = 0;
        this.isAvailable = true;
    }

    public boolean isAvailable() {
        return this.isAvailable;
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
