/**
 * 
 */
package se339.hw6;

/**
 * @author Andrew
 * 
 */
public class NonRentalException extends RuntimeException {

    public NonRentalException(String string) {
        super(string);
    }

    public NonRentalException() {
        super();
    }

    private static final long serialVersionUID = 1L;

}
