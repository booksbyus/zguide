if (zmq_poll (items, 2, 1000 * 1000) == -1)
    break;              //  Interrupted
