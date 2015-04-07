/**
 *
 */
package ex1_review;

/**
 * @author Andrew
 *
 */
public class Thing1 {
    public void doTheThingAsync(String[] args, IAsyncCallback cb) {
        cb.setResult(args[0]);
    }
}
