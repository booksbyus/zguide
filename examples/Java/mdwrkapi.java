/**
 * (c) 2011 Arkadiusz Orzechowski
 *
 * This file is part of ZGuide
 *
 * ZGuide is free software; you can redistribute it and/or modify it under
 * the terms of the Lesser GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * ZGuide is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Lesser GNU General Public License for more details.
 *
 * You should have received a copy of the Lesser GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
import java.util.Formatter;

import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;

/**
 * Majordomo Protocol Client API, Java version Implements the MDP/Worker spec at
 * http://rfc.zeromq.org/spec:7.
 * 
 * @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
public class mdwrkapi {

    private static final int HEARTBEAT_LIVENESS = 3; // 3-5 is reasonable

    private String broker;
    private ZContext ctx;
    private String service;

    private ZMQ.Socket worker; // Socket to broker
    private long heartbeatAt;// When to send HEARTBEAT
    private int liveness;// How many attempts left
    private int heartbeat = 2500;// Heartbeat delay, msecs
    private int reconnect = 2500; // Reconnect delay, msecs

    // Internal state
    private boolean expectReply = false; // false only at start

    private long timeout = 2500;
    private boolean verbose;// Print activity to stdout
    private Formatter log = new Formatter(System.out);

    // Return address, if any
    private ZFrame replyTo;

    public mdwrkapi(String broker, String service, boolean verbose) {
        assert (broker != null);
        assert (service != null);
        this.broker = broker;
        this.service = service;
        this.verbose = verbose;
        ctx = new ZContext();
        reconnectToBroker();
    }

    /**
     * Send message to broker If no msg is provided, creates one internally
     * 
     * @param command
     * @param option
     * @param msg
     */
    void sendToBroker(MDP command, String option, ZMsg msg) {
        msg = msg != null ? msg.duplicate() : new ZMsg();

        // Stack protocol envelope to start of message
        if (option != null)
            msg.addFirst(new ZFrame(option));

        msg.addFirst(command.newFrame());
        msg.addFirst(MDP.W_WORKER.newFrame());
        msg.addFirst(new ZFrame(new byte[0]));

        if (verbose) {
            log.format("I: sending %s to broker\n", command);
            msg.dump(log.out());
        }
        msg.send(worker);
    }

    /**
     * Connect or reconnect to broker
     */
    void reconnectToBroker() {
        if (worker != null) {
            ctx.destroySocket(worker);
        }
        worker = ctx.createSocket(ZMQ.DEALER);
        worker.connect(broker);
        if (verbose)
            log.format("I: connecting to broker at %s...\n", broker);

        // Register service with broker
        sendToBroker(MDP.W_READY, service, null);

        // If liveness hits zero, queue is considered disconnected
        liveness = HEARTBEAT_LIVENESS;
        heartbeatAt = System.currentTimeMillis() + heartbeat;

    }

    /**
     * Send reply, if any, to broker and wait for next request.
     */
    public ZMsg receive(ZMsg reply) {

        // Format and send the reply if we were provided one
        assert (reply != null || !expectReply);

        if (reply != null) {
            assert (replyTo != null);
            reply.wrap(replyTo);
            sendToBroker(MDP.W_REPLY, null, reply);
            reply.destroy();
        }
        expectReply = true;

        while (!Thread.currentThread().isInterrupted()) {
            // Poll socket for a reply, with timeout
            ZMQ.Poller items = ctx.getContext().poller();
            items.register(worker, ZMQ.Poller.POLLIN);
            if (items.poll(timeout * 1000) == -1)
                break; // Interrupted

            if (items.pollin(0)) {
                ZMsg msg = ZMsg.recvMsg(worker);
                if (msg == null)
                    break; // Interrupted
                if (verbose) {
                    log.format("I: received message from broker: \n");
                    msg.dump(log.out());
                }
                liveness = HEARTBEAT_LIVENESS;
                // Don't try to handle errors, just assert noisily
                assert (msg != null && msg.size() >= 3);

                ZFrame empty = msg.pop();
                assert (empty.getData().length == 0);
                empty.destroy();

                ZFrame header = msg.pop();
                assert (MDP.W_WORKER.frameEquals(header));
                header.destroy();

                ZFrame command = msg.pop();
                if (MDP.W_REQUEST.frameEquals(command)) {
                    // We should pop and save as many addresses as there are
                    // up to a null part, but for now, just save one...
                    replyTo = msg.unwrap();
                    command.destroy();
                    return msg; // We have a request to process
                } else if (MDP.W_HEARTBEAT.frameEquals(command)) {
                    // Do nothing for heartbeats
                } else if (MDP.W_DISCONNECT.frameEquals(command)) {
                    reconnectToBroker();
                } else {
                    log.format("E: invalid input message: \n");
                    msg.dump(log.out());
                }
                command.destroy();
                msg.destroy();
            } else if (--liveness == 0) {
                if (verbose)
                    log.format("W: disconnected from broker - retrying...\n");
                try {
                    Thread.sleep(reconnect);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt(); // Restore the
                                                        // interrupted status
                    break;
                }
                reconnectToBroker();

            }
            // Send HEARTBEAT if it's time
            if (System.currentTimeMillis() > heartbeatAt) {
                sendToBroker(MDP.W_HEARTBEAT, null, null);
                heartbeatAt = System.currentTimeMillis() + heartbeat;
            }

        }
        if (Thread.currentThread().isInterrupted())
            log.format("W: interrupt received, killing worker...\n");
        return null;
    }

    public void destroy() {
        ctx.destroy();
    }

    // ==============   getters and setters =================
    public int getHeartbeat() {
        return heartbeat;
    }

    public void setHeartbeat(int heartbeat) {
        this.heartbeat = heartbeat;
    }

    public int getReconnect() {
        return reconnect;
    }

    public void setReconnect(int reconnect) {
        this.reconnect = reconnect;
    }

}
