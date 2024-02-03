{
  Simple message queuing broker
  Same as request-reply broker but using shared queue proxy
  @author cpicanco <cpicanco@ufpa.br>
}
program msgqueue;

{$mode objfpc}{$H+}

uses zmq;

var
  context, frontend, backend: Pointer;
  rc: Integer;
begin
  context := zmq_ctx_new;

  // Socket facing clients
  frontend := zmq_socket(context, ZMQ_ROUTER);
  rc := zmq_bind(frontend, 'tcp://*:5559');
  Assert(rc = 0);

  // Socket facing services
  backend := zmq_socket(context, ZMQ_DEALER);
  rc := zmq_bind(backend, 'tcp://*:5560');
  Assert(rc = 0);

  // Start the proxy
  zmq_proxy(frontend, backend, nil);

  // We never get here…
  zmq_close(frontend);
  zmq_close(backend);
  zmq_ctx_destroy(context);
end.

