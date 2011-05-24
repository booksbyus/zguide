import org.zeromq.ZMQ;

public class Version
{

  public static void main ( final String[] args )
  {
    System.out.println ( String.format ( "Version string: %s, Version int: %d", 
                                         ZMQ.getVersionString (),
                                         ZMQ.getFullVersion () ) );
  }

}
