package guide;

import java.util.Arrays;

import org.zeromq.ZFrame;
import org.zeromq.ZMQ;

/**
 * Majordomo Protocol definitions, Java version
 */
public enum MDP
{

    /**
     * This is the version of MDP/Client we implement
     */
    C_CLIENT("MDPC01"),

    /**
     * This is the version of MDP/Worker we implement
     */
    W_WORKER("MDPW01"),

    // MDP/Server commands, as byte values
    W_READY(1),
    W_REQUEST(2),
    W_REPLY(3),
    W_HEARTBEAT(4),
    W_DISCONNECT(5);

    private final byte[] data;

    MDP(String value)
    {
        this.data = value.getBytes(ZMQ.CHARSET);
    }

    MDP(int value)
    { //watch for ints>255, will be truncated
        byte b = (byte) (value & 0xFF);
        this.data = new byte[] { b };
    }

    public ZFrame newFrame()
    {
        return new ZFrame(data);
    }

    public boolean frameEquals(ZFrame frame)
    {
        return Arrays.equals(data, frame.getData());
    }
}
