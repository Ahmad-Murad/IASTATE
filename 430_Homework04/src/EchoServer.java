import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class EchoServer
{

  public static void main(String[] args) throws Exception
  {
    // set to 4 for very verbose, 0 for no trace output
    System.setProperty("OtpConnection.trace", "4");

    // second argument is a "cookie", has to match the cookie of the sender
    OtpNode node = new OtpNode("echoserver@localhost", "thinmint");
    //OtpNode node = new OtpNode("echoserver", "thinmint");
    System.out.println(node.node());
    System.out.println("Created Erlang node: " + node.toString());

    OtpMbox mbox = node.createMbox("mymailbox");
    node.registerName("foobar", mbox);
 
    while (true)
    {
      try
      {
        OtpErlangObject o = mbox.receive();
        System.out.println("Received from mbox: " + o);
        if (o instanceof OtpErlangTuple)
        {
          OtpErlangTuple msg = (OtpErlangTuple) o;
          OtpErlangPid from = (OtpErlangPid) msg.elementAt(0);
          OtpErlangAtom atom = (OtpErlangAtom) msg.elementAt(1);
          System.out.println("Message: " + atom.atomValue());
          System.out.println("Replying to " + from);
          
          // echo the reply
          mbox.send(from, atom);
        }
        else
        {
          System.out.println("Message received: " + o);
        }
      }
      catch (Exception e)
      {
        System.out.println("" + e);
      }

    }

  }

}
