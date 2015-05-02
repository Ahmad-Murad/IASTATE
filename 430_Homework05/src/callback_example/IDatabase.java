package callback_example;

import java.io.IOException;
import java.util.concurrent.Future;

/**
 * Interface for potentially asynchronous operations on a service
 * that returns a String for a given key.
 */
public interface IDatabase
{
  /**
   * Procedural-style invocation blocks until result is returned.
   * @param key
   * @return
   */
  public String retrieve(int key) throws IOException;  
    
  /**
   * Async version that returns immediately and invokes the given
   * callback when result is ready or exception occurs. 
   * @param key
   * @param callback
   */
  public void retrieveAsync(int key, ICallback callback);
}
