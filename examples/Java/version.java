import org.zeromq.ZMQ;

// Lower case to match guide file naming convention
public class version
{

  public static void main(final String[] args)
  {
    System.out.println(String.format("Version string: %s, Version int: %d",
        ZMQ.getVersionString(),
        ZMQ.getFullVersion()));
  }

}
