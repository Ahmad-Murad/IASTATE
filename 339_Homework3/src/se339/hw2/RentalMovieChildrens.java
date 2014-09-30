/**
 * 
 */
package se339.hw2;

/**
 * @author aguibert
 * 
 */
public class RentalMovieChildrens extends Rentable
{
    private static final int OVERDUE_PERIOD = 2;

    public RentalMovieChildrens(String name, int daysRented, boolean isNewRelease) {
        super(name, daysRented, isNewRelease);
    }

    @Override
    public int daysOverdue() {
        return (daysRented > OVERDUE_PERIOD) ? (daysRented - OVERDUE_PERIOD) : 0;
    }

    @Override
    public double getCost() {
        if (isNewRelease())
            return 3.0 * getDaysRented();
        else
            return 1.5 + (daysOverdue() * 1.5);
    }

}
