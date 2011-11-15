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
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Formatter;
import java.util.HashMap;
import java.util.Map;

import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;

/**
 *  Majordomo Protocol broker
 *  A minimal implementation of http://rfc.zeromq.org/spec:7 and spec:8
 *
 *  @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
public class mdbroker {

    // We'd normally pull these from config data
    private static final String INTERNAL_SERVICE_PREFIX = "mmi.";
    private static final int HEARTBEAT_LIVENESS = 3; // 3-5 is reasonable
    private static final int HEARTBEAT_INTERVAL = 2500; // msecs
    private static final int HEARTBEAT_EXPIRY = HEARTBEAT_INTERVAL
            * HEARTBEAT_LIVENESS;

    // ---------------------------------------------------------------------

    /**
     * This defines a single service.
     */
    private static class Service {
        public final String name; // Service name
        Deque<ZMsg> requests; // List of client requests
        Deque<Worker> waiting; // List of waiting workers

        public Service(String name) {
            this.name = name;
            this.requests = new ArrayDeque<ZMsg>();
            this.waiting = new ArrayDeque<Worker>();
        }
    }

    /**
     * This defines one worker, idle or active.
     */
    private static class Worker {
        String identity;// Identity of worker
        ZFrame address;// Address frame to route to
        Service service; // Owning service, if known
        long expiry;// Expires at unless heartbeat

        public Worker(String identity, ZFrame address) {
            this.address = address;
            this.identity = identity;
            this.expiry = System.currentTimeMillis() + HEARTBEAT_INTERVAL
                    * HEARTBEAT_LIVENESS;
        }
    }

    // ---------------------------------------------------------------------

    private ZContext ctx;// Our context
    private ZMQ.Socket socket; // Socket for clients & workers

    private long heartbeatAt;// When to send HEARTBEAT
    private Map<String, Service> services;// known services
    private Map<String, Worker> workers;// known workers
    private Deque<Worker> waiting;// idle workers

    private boolean verbose = false;// Print activity to stdout
    private Formatter log = new Formatter(System.out);

    // ---------------------------------------------------------------------

    /**
     * Main method - create and start new broker.
     */
    public static void main(String[] args) {
        mdbroker broker = new mdbroker(args.length > 0 && "-v".equals(args[0]));
        // Can be called multiple times with different endpoints
        broker.bind("tcp://*:5555");
        broker.mediate();
    }

    /**
     * Initialize broker state.
     */
    public mdbroker(boolean verbose) {
        this.verbose = verbose;
        this.services = new HashMap<String, Service>();
        this.workers = new HashMap<String, Worker>();
        this.waiting = new ArrayDeque<Worker>();
        this.heartbeatAt = System.currentTimeMillis() + HEARTBEAT_INTERVAL;
        this.ctx = new ZContext();
        this.socket = ctx.createSocket(ZMQ.ROUTER);
    }

    // ---------------------------------------------------------------------

    /**
     * Main broker work happens here
     */
    public void mediate() {
        while (!Thread.currentThread().isInterrupted()) {
            ZMQ.Poller items = ctx.getContext().poller();
            items.register(socket, ZMQ.Poller.POLLIN);
            if (items.poll(HEARTBEAT_INTERVAL * 1000) == -1)
                break; // Interrupted
            if (items.pollin(0)) {
                ZMsg msg = ZMsg.recvMsg(socket);
                if (msg == null)
                    break; // Interrupted

                if (verbose) {
                    log.format("I: received message:\n");
                    msg.dump(log.out());
                }

                ZFrame sender = msg.pop();
                ZFrame empty = msg.pop();
                ZFrame header = msg.pop();

                if (MDP.C_CLIENT.frameEquals(header)) {
                    processClient(sender, msg);
                } else if (MDP.W_WORKER.frameEquals(header))
                    processWorker(sender, msg);
                else {
                    log.format("E: invalid message:\n");
                    msg.dump(log.out());
                    msg.destroy();
                }

                sender.destroy();
                empty.destroy();
                header.destroy();

            }
            purgeWorkers();
            sendHeartbeats();
        }
        destroy(); // interrupted
    }

    /**
     * Disconnect all workers, destroy context.
     */
    private void destroy() {
        Worker[] deleteList = workers.entrySet().toArray(new Worker[0]);
        for (Worker worker : deleteList) {
            deleteWorker(worker, true);
        }
        ctx.destroy();
    }

    /**
     * Process a request coming from a client.
     */
    private void processClient(ZFrame sender, ZMsg msg) {
        assert (msg.size() >= 2); // Service name + body
        ZFrame serviceFrame = msg.pop();
        // Set reply return address to client sender
        msg.wrap(sender.duplicate());
        if (serviceFrame.toString().startsWith(INTERNAL_SERVICE_PREFIX))
            serviceInternal(serviceFrame, msg);
        else
            dispatch(requireService(serviceFrame), msg);
        serviceFrame.destroy();
    }

    /**
     * Process message sent to us by a worker.
     */
    private void processWorker(ZFrame sender, ZMsg msg) {
        assert (msg.size() >= 1); // At least, command

        ZFrame command = msg.pop();

        boolean workerReady = workers.containsKey(sender.strhex());

        Worker worker = requireWorker(sender);

        if (MDP.W_READY.frameEquals(command)) {
            // Not first command in session || Reserved service name
            if (workerReady
                    || sender.toString().startsWith(INTERNAL_SERVICE_PREFIX))
                deleteWorker(worker, true);
            else {
                // Attach worker to service and mark as idle
                ZFrame serviceFrame = msg.pop();
                worker.service = requireService(serviceFrame);
                workerWaiting(worker);
                serviceFrame.destroy();
            }
        } else if (MDP.W_REPLY.frameEquals(command)) {
            if (workerReady) {
                // Remove & save client return envelope and insert the
                // protocol header and service name, then rewrap envelope.
                ZFrame client = msg.unwrap();
                msg.addFirst(worker.service.name);
                msg.addFirst(MDP.C_CLIENT.newFrame());
                msg.wrap(client);
                msg.send(socket);
                workerWaiting(worker);
            } else {
                deleteWorker(worker, true);
            }
        } else if (MDP.W_HEARTBEAT.frameEquals(command)) {
            if (workerReady) {
                worker.expiry = System.currentTimeMillis() + HEARTBEAT_EXPIRY;
            } else {
                deleteWorker(worker, true);
            }
        } else if (MDP.W_DISCONNECT.frameEquals(command))
            deleteWorker(worker, false);
        else {
            log.format("E: invalid message:\n");
            msg.dump(log.out());
        }
        msg.destroy();
    }

    /**
     * Deletes worker from all data structures, and destroys worker.
     */
    private void deleteWorker(Worker worker, boolean disconnect) {
        assert (worker != null);
        if (disconnect) {
            sendToWorker(worker, MDP.W_DISCONNECT, null, null);
        }
        if (worker.service != null)
            worker.service.waiting.remove(worker);
        workers.remove(worker);
        worker.address.destroy();
    }

    /**
     * Finds the worker (creates if necessary).
     */
    private Worker requireWorker(ZFrame address) {
        assert (address != null);
        String identity = address.strhex();
        Worker worker = workers.get(identity);
        if (worker == null) {
            worker = new Worker(identity, address.duplicate());
            workers.put(identity, worker);
            if (verbose)
                log.format("I: registering new worker: %s\n", identity);
        }
        return worker;
    }

    /**
     * Locates the service (creates if necessary).
     */
    private Service requireService(ZFrame serviceFrame) {
        assert (serviceFrame != null);
        String name = serviceFrame.toString();
        Service service = services.get(name);
        if (service == null) {
            service = new Service(name);
            services.put(name, service);
        }
        return service;
    }

    /**
     * Bind broker to endpoint, can call this multiple times. We use a single
     * socket for both clients and workers.
     */
    private void bind(String endpoint) {
        socket.bind(endpoint);
        log.format("I: MDP broker/0.1.1 is active at %s\n", endpoint);
    }

    /**
     * Handle internal service according to 8/MMI specification
     */
    private void serviceInternal(ZFrame serviceFrame, ZMsg msg) {
        String returnCode = "501";
        if ("mmi.service".equals(serviceFrame.toString())) {
            String name = msg.peekLast().toString();
            returnCode = services.containsKey(name) ? "200" : "400";
        }
        msg.peekLast().reset(returnCode.getBytes());
        // Remove & save client return envelope and insert the
        // protocol header and service name, then rewrap envelope.
        ZFrame client = msg.unwrap();
        msg.addFirst(serviceFrame.duplicate());
        msg.addFirst(MDP.C_CLIENT.newFrame());
        msg.wrap(client);
        msg.send(socket);
    }

    /**
     * Send heartbeats to idle workers if it's time
     */
    public synchronized void sendHeartbeats() {
        // Send heartbeats to idle workers if it's time
        if (System.currentTimeMillis() >= heartbeatAt) {
            for (Worker worker : waiting) {
                sendToWorker(worker, MDP.W_HEARTBEAT, null, null);
            }
            heartbeatAt = System.currentTimeMillis() + HEARTBEAT_INTERVAL;
        }
    }

    /**
     * Look for & kill expired workers. Workers are oldest to most recent, so we
     * stop at the first alive worker.
     */
    public synchronized void purgeWorkers() {
        for (Worker w = waiting.peekFirst(); w != null
                && w.expiry < System.currentTimeMillis(); w = waiting
                .peekFirst()) {
            log.format("I: deleting expired worker: %s\n", w.identity);
            deleteWorker(waiting.pollFirst(), false);
        }
    }

    /**
     * This worker is now waiting for work.
     */
    public synchronized void workerWaiting(Worker worker) {
        // Queue to broker and service waiting lists
        waiting.addLast(worker);
        worker.service.waiting.addLast(worker);
        worker.expiry = System.currentTimeMillis() + HEARTBEAT_EXPIRY;
        dispatch(worker.service, null);
    }

    /**
     * Dispatch requests to waiting workers as possible
     */
    private void dispatch(Service service, ZMsg msg) {
        assert (service != null);
        if (msg != null)// Queue message if any
            service.requests.offerLast(msg);
        purgeWorkers();
        while (!service.waiting.isEmpty() && !service.requests.isEmpty()) {
            msg = service.requests.pop();
            Worker worker = service.waiting.pop();
            waiting.remove(worker);
            sendToWorker(worker, MDP.W_REQUEST, null, msg);
            msg.destroy();
        }
    }

    /**
     * Send message to worker. If message is provided, sends that message. Does
     * not destroy the message, this is the caller's job.
     */
    public void sendToWorker(Worker worker, MDP command, String option,
            ZMsg msgp) {

        ZMsg msg = msgp == null ? new ZMsg() : msgp.duplicate();

        // Stack protocol envelope to start of message
        if (option != null)
            msg.addFirst(new ZFrame(option));
        msg.addFirst(command.newFrame());
        msg.addFirst(MDP.W_WORKER.newFrame());

        // Stack routing envelope to start of message
        msg.wrap(worker.address.duplicate());
        if (verbose) {
            log.format("I: sending %s to worker\n", command);
            msg.dump(log.out());
        }
        msg.send(socket);
    }
}
