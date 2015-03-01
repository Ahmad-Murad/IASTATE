package message_passing;

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
