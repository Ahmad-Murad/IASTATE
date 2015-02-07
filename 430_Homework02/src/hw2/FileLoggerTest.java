/**
 *
 */
package hw2;

import org.junit.Test;

/**
 * @author Andrew
 *
 */
public class FileLoggerTest {

    @Test
    public void test() throws Exception {
        FileLogger log = new FileLogger("myfile1");
        log.log("Message 1");
        log.log("Message 2");
        log.log("Message 3");
        log.log("Message 4");

        Thread.sleep(5000);

        for (int i = 0; i < 11; i++)
            log.log("Message " + i);

        Thread.sleep(10000);
    }

}
