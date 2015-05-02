package callback_example;

/**
 * General callback for an async operation with a String result.
 */
public interface ICallback
{
  public void asyncResult(String result);
  public void asyncException(Exception e);
}
