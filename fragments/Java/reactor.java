ZLoop reactor = new ZLoop ();
reactor.poller (this.backend, handler, this);
reactor.start ();
reactor.destroy ();
