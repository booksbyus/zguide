package guide;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.UUID;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;

public class kvmsg
{
    //  Keys are short strings
    private static final int KVMSG_KEY_MAX = 255;

    //  Message is formatted on wire as 4 frames:
    //  frame 0: getKey (0MQ string)
    //  frame 1: getSequence (8 bytes, network order)
    //  frame 2: uuid (blob, 16 bytes)
    //  frame 3: properties (0MQ string)
    //  frame 4: body (blob)
    private static final int FRAME_KEY    = 0;
    private static final int FRAME_SEQ    = 1;
    private static final int FRAME_UUID   = 2;
    private static final int FRAME_PROPS  = 3;
    private static final int FRAME_BODY   = 4;
    private static final int KVMSG_FRAMES = 5;

    //  Presence indicators for each frame
    private boolean[] present = new boolean[KVMSG_FRAMES];
    //  Corresponding 0MQ message frames, if any
    private byte[][] frame = new byte[KVMSG_FRAMES][];
    //  Key, copied into safe string
    private String key;
    //  List of properties, as name=value strings
    private Properties props;
    private int        props_size;

    //  .split property encoding
    //  These two helpers serialize a list of properties to and from a
    //  message frame:

    private void encodeProps()
    {
        ByteBuffer msg = ByteBuffer.allocate(props_size);
        for (Entry<Object, Object> o : props.entrySet()) {
            String prop = o.getKey().toString() + "=" + o.getValue().toString() + "\n";
            msg.put(prop.getBytes(ZMQ.CHARSET));
        }
        present[FRAME_PROPS] = true;
        frame[FRAME_PROPS] = msg.array();
    }

    private void decodeProps()
    {
        byte[] msg = frame[FRAME_PROPS];
        props_size = msg.length;
        props.clear();

        if (msg.length == 0)
            return;

        System.out.println("" + msg.length + " :" + new String(msg, ZMQ.CHARSET));
        for (String prop : new String(msg, ZMQ.CHARSET).split("\n")) {
            String[] split = prop.split("=");
            props.setProperty(split[0], split[1]);
        }
    }

    //  .split constructor and destructor
    //  Here are the constructor and destructor for the class:

    //  Constructor, takes a getSequence number for the new kvmsg instance:
    public kvmsg(long sequence)
    {
        props = new Properties();
        setSequence(sequence);
    }

    public void destroy()
    {
    }

    //  .split recv method
    //  This method reads a getKey-value message from the socket and returns a
    //  new {{kvmsg}} instance:
    public static kvmsg recv(Socket socket)
    {
        //  This method is almost unchanged from kvsimple
        //  .skip
        assert (socket != null);
        kvmsg self = new kvmsg(0);

        //  Read all frames off the wire, reject if bogus
        int frameNbr;
        for (frameNbr = 0; frameNbr < KVMSG_FRAMES; frameNbr++) {
            //zmq_msg_init (&self->frame [frameNbr]);
            self.present[frameNbr] = true;
            if ((self.frame[frameNbr] = socket.recv(0)) == null) {
                self.destroy();
                break;
            }
            //  Verify multipart framing
            boolean rcvmore = (frameNbr < KVMSG_FRAMES - 1) ? true : false;
            if (socket.hasReceiveMore() != rcvmore) {
                self.destroy();
                break;
            }
        }
        //  .until
        self.decodeProps();
        return self;
    }

    //  Send getKey-value message to socket; any empty frames are sent as such.
    public void send(Socket socket)
    {
        assert (socket != null);

        encodeProps();
        //  The rest of the method is unchanged from kvsimple
        //  .skip
        int frameNbr;
        for (frameNbr = 0; frameNbr < KVMSG_FRAMES; frameNbr++) {
            byte[] copy = ZMQ.MESSAGE_SEPARATOR;
            if (present[frameNbr])
                copy = frame[frameNbr];
            socket.send(copy, (frameNbr < KVMSG_FRAMES - 1) ? ZMQ.SNDMORE : 0);
        }
    }

    //  .until

    //  .split dup method
    //  This method duplicates a {{kvmsg}} instance, returns the new instance:
    public kvmsg dup()
    {
        kvmsg kvmsg = new kvmsg(0);
        int frameNbr;
        for (frameNbr = 0; frameNbr < KVMSG_FRAMES; frameNbr++) {
            if (present[frameNbr]) {
                kvmsg.frame[frameNbr] = new byte[frame[frameNbr].length];
                System.arraycopy(frame[frameNbr], 0, kvmsg.frame[frameNbr], 0, frame[frameNbr].length);
                kvmsg.present[frameNbr] = true;
            }
        }
        kvmsg.props_size = props_size;
        kvmsg.props.putAll(props);
        return kvmsg;
    }
    //  The getKey, getSequence, body, and size methods are the same as in kvsimple.
    //  .skip

    //  Return getKey from last read message, if any, else NULL
    public String getKey()
    {
        if (present[FRAME_KEY]) {
            if (key == null) {
                int size = frame[FRAME_KEY].length;
                if (size > KVMSG_KEY_MAX)
                    size = KVMSG_KEY_MAX;
                byte[] buf = new byte[size];
                System.arraycopy(frame[FRAME_KEY], 0, buf, 0, size);
                key = new String(buf, ZMQ.CHARSET);
            }
            return key;
        }
        else return null;
    }

    //  Set message getKey as provided
    public void setKey(String key)
    {
        byte[] msg = new byte[key.length()];
        System.arraycopy(key.getBytes(ZMQ.CHARSET), 0, msg, 0, key.length());
        frame[FRAME_KEY] = msg;
        present[FRAME_KEY] = true;
    }

    //  Set message getKey using printf format
    public void fmtKey(String fmt, Object... args)
    {
        setKey(String.format(fmt, args));
    }

    //  Return getSequence nbr from last read message, if any
    public long getSequence()
    {
        if (present[FRAME_SEQ]) {
            assert (frame[FRAME_SEQ].length == 8);
            ByteBuffer source = ByteBuffer.wrap(frame[FRAME_SEQ]);
            return source.getLong();
        }
        else return 0;
    }

    //  Set message getSequence number
    public void setSequence(long sequence)
    {
        ByteBuffer msg = ByteBuffer.allocate(8);

        msg.putLong(sequence);

        present[FRAME_SEQ] = true;
        frame[FRAME_SEQ] = msg.array();
    }

    //  Return body from last read message, if any, else NULL
    public byte[] body()
    {
        if (present[FRAME_BODY])
            return frame[FRAME_BODY];
        else return null;
    }

    //  Set message body
    public void setBody(byte[] body)
    {
        byte[] msg = new byte[body.length];
        System.arraycopy(body, 0, msg, 0, body.length);
        frame[FRAME_BODY] = msg;
        present[FRAME_BODY] = true;
    }

    //  Set message body using printf format
    public void fmtBody(String fmt, Object... args)
    {
        setBody(String.format(fmt, args).getBytes(ZMQ.CHARSET));
    }

    //  Return body size from last read message, if any, else zero
    public int size()
    {
        if (present[FRAME_BODY])
            return frame[FRAME_BODY].length;
        else return 0;
    }
    //  .until

    //  .split UUID methods
    //  These methods get and set the UUID for the getKey-value message:
    public byte[] UUID()
    {
        if (present[FRAME_UUID])
            return frame[FRAME_UUID];
        else return null;
    }

    //  Sets the UUID to a randomly generated value
    public void setUUID()
    {
        byte[] msg = UUID.randomUUID().toString().getBytes(ZMQ.CHARSET);
        present[FRAME_UUID] = true;
        frame[FRAME_UUID] = msg;
    }

    //  .split property methods
    //  These methods get and set a specified message property:

    //  Get message property, return "" if no such property is defined.
    public String getProp(String name)
    {
        return props.getProperty(name, "");
    }

    //  Set message property. Property name cannot contain '='. Max length of
    //  value is 255 chars.
    public void setProp(String name, String fmt, Object... args)
    {
        String value = String.format(fmt, args);
        Object old = props.setProperty(name, value);
        if (old != null)
            props_size -= old.toString().length();
        else props_size += name.length() + 2;
        props_size += value.length();
    }

    //  .split store method
    //  This method stores the getKey-value message into a hash map, unless
    //  the getKey and value are both null. It nullifies the {{kvmsg}} reference
    //  so that the object is owned by the hash map, not the caller:

    public void store(Map<String, kvmsg> hash)
    {
        if (size() > 0) {
            if (present[FRAME_KEY] && present[FRAME_BODY]) {
                hash.put(getKey(), this);
            }
        }
        else hash.remove(getKey());
    }

    //  .split dump method
    //  This method extends the {{kvsimple}} implementation with support for
    //  message properties:

    public void dump()
    {
        int size = size();
        byte[] body = body();
        System.err.printf("[seq:%d]", getSequence());
        System.err.printf("[getKey:%s]", getKey());
        //  .until
        System.err.printf("[size:%d] ", size);
        System.err.printf("[");
        for (String key : props.stringPropertyNames()) {
            System.err.printf("%s=%s;", key, props.getProperty(key));
        }
        System.err.printf("]");

        //  .skip
        for (int charNbr = 0; charNbr < size; charNbr++)
            System.err.printf("%02X", body[charNbr]);
        System.err.printf("\n");
    }

    //  .until

    //  .split test method
    //  This method is the same as in {{kvsimple}} with added support
    //  for the uuid and property features of {{kvmsg}}:
    public void test(boolean verbose)
    {
        System.out.printf(" * kvmsg: ");

        //  Prepare our context and sockets
        try (ZContext ctx = new ZContext()) {
            Socket output = ctx.createSocket(SocketType.DEALER);
            output.bind("ipc://kvmsg_selftest.ipc");
            Socket input = ctx.createSocket(SocketType.DEALER);
            input.connect("ipc://kvmsg_selftest.ipc");

            Map<String, kvmsg> kvmap = new HashMap<String, kvmsg>();

            //  .until
            //  Test send and receive of simple message
            kvmsg kvmsg = new kvmsg(1);
            kvmsg.setKey("getKey");
            kvmsg.setUUID();
            kvmsg.setBody("body".getBytes(ZMQ.CHARSET));
            if (verbose)
                kvmsg.dump();
            kvmsg.send(output);
            kvmsg.store(kvmap);

            kvmsg = guide.kvmsg.recv(input);
            if (verbose)
                kvmsg.dump();
            assert (kvmsg.getKey().equals("getKey"));
            kvmsg.store(kvmap);

            //  Test send and receive of message with properties
            kvmsg = new kvmsg(2);
            kvmsg.setProp("prop1", "value1");
            kvmsg.setProp("prop2", "value1");
            kvmsg.setProp("prop2", "value2");
            kvmsg.setKey("getKey");
            kvmsg.setUUID();
            kvmsg.setBody("body".getBytes(ZMQ.CHARSET));
            assert (kvmsg.getProp("prop2").equals("value2"));
            if (verbose)
                kvmsg.dump();
            kvmsg.send(output);
            kvmsg.destroy();

            kvmsg = guide.kvmsg.recv(input);
            if (verbose)
                kvmsg.dump();
            assert (kvmsg.key.equals("getKey"));
            assert (kvmsg.getProp("prop2").equals("value2"));
            kvmsg.destroy();
        }

        System.out.printf("OK\n");
    }
    //  .until
}
