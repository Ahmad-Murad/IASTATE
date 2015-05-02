/**
 *
 */
package server;

import Ice.Current;
import dbexample.NoSuchEntryException;
import dbexample._DatabaseDisp;

/**
 * @author Andrew
 *
 */
public class DatabaseI extends _DatabaseDisp {

    @Override
    public String retrieve(int key, Current __current) throws NoSuchEntryException {
        return FakeDatabase.retrieve(key);
    }

}
