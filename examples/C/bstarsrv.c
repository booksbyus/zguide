typedef enum
{
    state_pending = 1,   //  Waiting for peer to connect
    state_active  = 2,   //  Active - accepting connections
    state_passive = 3    //  Passive - not accepting connections
} state;

typedef enum
{
    event_peer_pending   = 1,  //  HA peer became pending
    event_peer_active    = 2,  //  HA peer became active
    event_peer_passive   = 3,  //  HA peer became passive
    event_new_connection = 4   //  Attempt to create a new connection
} event;

- verify arg1 -1server OR -2server

    amq_peering_t
        *peering;                       //  The peering to the other HA peer
    Bool
        enabled,                        //  If FALSE, broker is standalone
        primary;                        //  TRUE = primary, FALSE = backup
    long
        timeout;                        //  Failover timeout in usec
    state
        state;                          //  State of failover FSM
    apr_time_t
        last_peer_time;                 //  Time when peer state arrived lately
                                        //  If this time is older than the failover
                                        //  timeout, the peer is considered dead
    amq_exchange_t
        *status_exchange;               //  amq.status exchange


    char
        *backup,                        //  Backup to connect to
        *primary;                       //  Primary to connect to


    self->status_exchange = amq_exchange_table_search (
        amq_broker->exchange_table, "amq.status");
    assert (self->status_exchange);
    backup  = amq_server_config_backup  (amq_server_config);
    primary = amq_server_config_primary (amq_server_config);

    //  All timeouts are represented internally as usec for SMT
    self->timeout = amq_server_config_failover_timeout (amq_server_config)
                  * 1000000;

    //  Check configuration is sane
    if (*backup && *primary)
        icl_console_print ("E: don't set both --backup and --primary");
    else
    if (*backup) {
        self->enabled = TRUE;
        self->primary = TRUE;
    }
    else
    if (*primary)
        self->enabled = TRUE;
    //  Enable failover if requested
    if (self->enabled) {
        smt_log_print (amq_broker->alert_log, "I: failover enabled, acting as %s",
            self->primary? "master": "slave");
        smt_log_print (amq_broker->alert_log, "I: failover: waiting for %s...",
            self->primary? "backup (slave)": "primary (master)");

        self->state = state_pending;
        self->last_peer_time = 0;

        //  Start peering
        self->peering = amq_peering_new ( *primary? primary: backup,
        amq_peering_start (self->peering);

        //  Subscribe for failover peer's state notifications
        amq_peering_bind (self->peering,
            self->primary? "failover.backup": "failover.primary", NULL);

        //  Start monitoring failover peer state
        amq_failover_start_monitoring (self);
    }
    //  Else, configuration not OK, or failover disabled
    else
        smt_log_print (amq_broker->alert_log, "I: no failover defined, READY as stand-alone");
</method>

<method name = "start_monitoring" template = "async function">
    <action>
    smt_timer_request_delay (self->thread, self->timeout / 2, monitor_event);
    </action>
</method>

<method name = "destroy">
    if (self->peering)
        amq_peering_stop (self->peering);
</method>

<method name = "send state" template = "function">
    <local>
    icl_shortstr_t
        state;
    amq_content_basic_t
        *content;
    </local>
    //
    content = amq_content_basic_new ();
    amq_content_basic_set_routing_key (content, "amq.status",
        self->primary? "failover.primary" : "failover.backup", 0);
    icl_shortstr_fmt (state, "%d", self->state);
    amq_content_basic_set_body (content,
        icl_mem_strdup (state), strlen (state) + 1, icl_mem_free);
    amq_exchange_publish (self->status_exchange,
        NULL, content, FALSE, FALSE, AMQ_CONNECTION_GROUP_SUPER);
    amq_content_basic_unlink (&content);
</method>

<method name = "execute" return = "rc" template = "function">
    <argument name = "event" type = "int" />
    <declare name = "rc" type = "int" default = "0" />
    //
    switch (self->state) {
      case state_pending:
        switch (event) {
          case event_peer_pending:
            if (self->primary) {
                self->state = state_active;
                smt_log_print (amq_broker->alert_log,
                    "I: failover: connected to backup (slave), READY as master");
            }
            break;
          case event_peer_active:
            self->state = state_passive;
            smt_log_print (amq_broker->alert_log,
                "I: failover: connected to %s (master), READY as slave",
                self->primary? "backup": "primary");
            break;
          case event_peer_passive:
            //  Do nothing; wait while peer switches to active
            break;
          case event_new_connection:
            //  If pending, accept connection only if primary peer
            rc = (self->primary);
            break;
          default:
            assert (0);
        }
        break;

      case state_active:
        switch (event) {
          case event_peer_pending:
            //  Do nothing; slave is starting
            break;
          case event_peer_active:
            //  No way to have two masters - that would mean split-brain
            smt_log_print (amq_broker->alert_log,
                "E: failover: fatal error - dual masters detected, aborting");
            assert (0);
            break;
          case event_peer_passive:
            //  Do nothing; everything is OK
            break;
          case event_new_connection:
            //  Active state, we do accept new connections
            rc = 1;
            break;
          default:
            assert (0);
        }
        break;

      case state_passive:
        switch (event) {
          case event_peer_pending:
            //  The peer is restarting; become active (peer will become passive)
            self->state = state_active;
            smt_log_print (amq_broker->alert_log,
                "I: failover: %s (slave) is restarting, READY as master",
                self->primary? "backup": "primary");
            break;
          case event_peer_active:
            //  Do nothing; everything is OK
            break;
          case event_peer_passive:
            //  No way to have two passives - cluster would be non-responsive
            smt_log_print (amq_broker->alert_log,
                "E: failover: fatal error - dual slaves, aborting");
            assert (0);
            break;
          case event_new_connection:
            //  Peer becomes master if timeout has passed
            //  It's the connection request that triggers the failover
            if (smt_time_now () - self->last_peer_time > self->timeout) {
                //  If peer is dead, switch to the active state
                self->state = state_active;
                smt_log_print (amq_broker->alert_log,
                    "I: failover: failover successful, READY as master");
                rc = 1;                 //  Accept the request, then
            }
            else
                //  If peer is alive, reject connections
                rc = 0;
            break;
          default:
            assert (0);
        }
        break;

      default:
        assert (0);
    }
</method>

<event name = "monitor">
    <action>
    //  Send state notification to failover peer
    //  We do this unconditionally; if the failover peer is not present the
    //  message will be discarded (it's sent to the status exchange).
    amq_failover_send_state (self);

    //  Schedule new monitoring event
    smt_timer_request_delay (self->thread, self->timeout / 2, monitor_event);
    </action>
</event>

<private name = "header">
static int
    s_content_handler (void *vself, amq_peering_t *peering, amq_peer_method_t *peer_method);
</private>

<private name = "footer">
static int
s_content_handler (
    void *vself,
    amq_peering_t *peering,
    amq_peer_method_t *peer_method)
{
    amq_failover_t
        *self = (amq_failover_t *) vself;
    asl_reader_t
        reader;
    ipr_bucket_t
        *body;
    state
        state;
    event
        event;

    assert (peer_method->class_id == AMQ_PEER_BASIC);
    assert (peer_method->method_id == AMQ_PEER_BASIC_DELIVER);

    //  Status from other HAC party received
    self->last_peer_time = smt_time_now ();

    //  Parse content
    amq_content_basic_set_reader (peer_method->content, &reader, 4096);
    body = amq_content_basic_replay_body (peer_method->content, &reader);
    assert (body);
    state = atoi ((char *) body->data);
    assert (state != 0);
    ipr_bucket_destroy (&body);

    //  Convert peer's state to FSM event
    switch (state) {
    case state_pending:
        event = event_peer_pending;
        break;
    case state_active:
        event = event_peer_active;
        break;
    case state_passive:
        event = event_peer_passive;
        break;
    default:
        assert (0);
    }
    //  Run the FSM
    self_execute (self, event);

    return 0;
}
</private>

