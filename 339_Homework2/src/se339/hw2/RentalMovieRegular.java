/**
 * 
 */
package se339.hw2;

/**
 * @author aguibert
 * 
 */
public class RentalMovieRegular extends Rentable
{
    private static final int OVERDUE_PERIOD = 2;

    public RentalMovieRegular(String name, int daysRented, boolean isNewRelease) {
        super(name, daysRented, isNewRelease);
    }

    @Override
    public double getCost() {
        if (isNewRelease())
            return 3.0 * getDaysRented();
        else
            return 2.0 + (daysOverdue() * 1.5);
    }

    @Override
    public int daysOverdue() {
        return (daysRented > OVERDUE_PERIOD) ? (daysRented - OVERDUE_PERIOD) : 0;
    }
}
