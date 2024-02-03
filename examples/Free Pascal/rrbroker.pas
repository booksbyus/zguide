{
  Simple request-reply broker
  @author cpicanco <cpicanco@ufpa.br>
}
program rrbroker;

{$mode objfpc}{$H+}

uses SysUtils, zmq, zmq.helpers;

const
  forever = False;

var
  context, frontend, backend: Pointer;
  items : array [0..1] of zmq_pollitem_t;

  message : zmq_msg_t;
  more : integer;
begin
  //  Prepare our context and sockets
  context  := zmq_ctx_new;
  frontend := zmq_socket(context, ZMQ_ROUTER);
  backend  := zmq_socket(context, ZMQ_DEALER);
  zmq_bind(frontend, 'tcp://*:5559');
  zmq_bind(backend,  'tcp://*:5560');

  //  Initialize poll array
  with items[0] do
    begin
      socket := frontend;
      fd := 0;
      events := ZMQ_POLLIN;
      revents := 0;
    end;
  with items[1] do
    begin
      socket := backend;
      fd := 0;
      events := ZMQ_POLLIN;
      revents := 0;
    end;

  //  Switch messages between sockets
  repeat
    message := Default(zmq_msg_t);
    zmq_poll(items[0], 2, -1);
    if (items[0].revents and ZMQ_POLLIN) > 0 then
      repeat
        //  Process all parts of the message
        zmq_msg_init(message);
        zmq_msg_recv(message, frontend, 0);
        more := zmq_msg_more(message);
        if more = 0 then { do nothing } else more := ZMQ_SNDMORE;
        zmq_msg_send(message, backend, more);
        zmq_msg_close(message);
        if more = 0 then break; //  Last message part
      until forever;

    if (items[1].revents and ZMQ_POLLIN) > 0 then
      repeat
        //  Process all parts of the message
        zmq_msg_init(message);
        zmq_msg_recv(message, backend, 0);
        more := zmq_msg_more(message);
        if more = 0 then { do nothing } else more := ZMQ_SNDMORE;
        zmq_msg_send(message, frontend, more);
        zmq_msg_close(message);
        if more = 0 then break; //  Last message part
      until forever;
  until forever;

  //  We never get here, but clean up anyhow
  zmq_close(frontend);
  zmq_close(backend);
  zmq_ctx_destroy(context);
end.

