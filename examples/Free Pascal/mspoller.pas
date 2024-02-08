{
  Reading from multiple sockets
  This version uses zmq_poll()
  @author cpicanco <cpicanco@ufpa.br>
}
program mspoller;

{$MODE objfpc}{$H+}

uses zmq, zmq.helpers;

var
  context, receiver, subscriber: Pointer;
  filter : string = '10001';
  items : array [0..1] of zmq_pollitem_t;
  msg : array [0..255] of Char;
  size: Integer;
begin
  //  Connect to task ventilator
  context := zmq_ctx_new;
  receiver := zmq_socket(context, ZMQ_PULL);
  zmq_connect(receiver, 'tcp://localhost:5557');

  //  Connect to weather server
  subscriber := zmq_socket(context, ZMQ_SUB);
  zmq_connect(subscriber, 'tcp://localhost:5556');
  zmq_setsockopt(subscriber, ZMQ_SUBSCRIBE, @filter[1], Length(filter));

  //  Process messages from both sockets
  repeat
    FillChar(msg, SizeOf(msg), 0);
    with items[0] do
    begin
      socket := receiver;
      fd := 0;
      events := ZMQ_POLLIN;
      revents := 0;
    end;
    with items[1] do
    begin
      socket := subscriber;
      fd := 0;
      events := ZMQ_POLLIN;
      revents := 0;
    end;

    zmq_poll(items[0], Length(items), -1);
    if (items[0].revents and ZMQ_POLLIN) > 0 then
    begin
      size := zmq_recv(receiver,   @msg, 255, 0);
      if (size <> -1) then ; //  Process task
    end;
    if (items[1].revents and ZMQ_POLLIN) > 0 then
    begin
      size := zmq_recv(subscriber, @msg, 255, 0);
      if (size <> -1) then ; //  Process weather update
    end;
  until False;
  zmq_close(subscriber);
  zmq_ctx_destroy(context);
end.
