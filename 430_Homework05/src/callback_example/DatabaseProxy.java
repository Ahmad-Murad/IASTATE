package callback_example;


import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * Implementation of the IDatabase interface obtains results
 * from server on port 2222 as in homework 1.
 */
public class DatabaseProxy implements IDatabase
{
  public static final String HOST = "localhost";
  public static final int PORT = 2222;
  
  @Override
  public String retrieve(int key) throws IOException
  {
    return getResultFromDB(key);
  }



  @Override
  public void retrieveAsync(final int key, final ICallback callback)
  {
    Runnable r = new Runnable()
    {
      public void run()
      {
        try
        {
          String result = getResultFromDB(key);
          callback.asyncResult(result);
        }
        catch (IOException e)
        {
          callback.asyncException(e);
        }
      }
    };
    new Thread(r).start();
    
    // returns immediately
  }
  
  private String getResultFromDB(int key) throws IOException
  {
    Socket s = null;
    try
    {
      // open a connection to the server
      s = new Socket(HOST, PORT);

      // for line-oriented output we use a PrintWriter
      PrintWriter pw = new PrintWriter(s.getOutputStream());
      pw.println("" + key);
      pw.flush();  // don't forget to flush...    

      // read response, which we expect to be line-oriented text
      Scanner scanner = new Scanner(s.getInputStream());
      String result = scanner.nextLine();
      return result;
    }
    finally
    {
      // be sure streams are closed
      try
      {
        s.close();
      }
      catch (IOException ignore){}
    }
  }
}


