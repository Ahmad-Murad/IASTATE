package callback_example;

public class NoSuchEntryException extends Exception
{
  public NoSuchEntryException()
  {
    super();
  }
  
  public NoSuchEntryException(String msg)
  {
    super(msg);
  }
}
