/**
 * 
 */
package se339.hw2;

/**
 * @author aguibert
 * 
 */
public abstract class Rentable implements Product
{
    protected final String name;
    protected final int daysRented;
    protected final boolean isNewRelease;

    public Rentable(String name, int daysRented, boolean isNewRelease) {
        this.name = name;
        this.daysRented = daysRented;
        this.isNewRelease = isNewRelease;
    }

    @Override
    public final String getName() {
        return this.name;
    }

    @Override
    public abstract double getCost();

    public boolean isNewRelease() {
        return this.isNewRelease;
    }

    public abstract int daysOverdue();

    public int getDaysRented() {
        return this.daysRented;
    }

    public int getFrequentRenterPoints() {
        return (isNewRelease && daysRented > 1) ? 2 : 1;
    }

    @Override
    public String toString() {
        return '\t' + getName() + '\t' + String.valueOf(getCost());
    }
}
