{
  Reading from multiple sockets
  This version uses a simple recv loop
  @author cpicanco <cpicanco@ufpa.br>
}
program msread;

uses SysUtils, zmq;

var
  context, receiver, subscriber: Pointer;
  filter : string = '10001';

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
  //  We prioritize traffic from the task ventilator
  repeat
    FillChar(msg, SizeOf(msg), 0);
    repeat
      size := zmq_recv(receiver, @msg, 255, ZMQ_DONTWAIT);
      // Process task
    until size = -1;

    repeat
      size := zmq_recv(subscriber, @msg, 255, ZMQ_DONTWAIT);
      // Process weather update
    until size = -1;

    //  No activity, so sleep for 1 msec
    Sleep(1);
  until False;
  zmq_close(receiver);
  zmq_close(subscriber);
  zmq_ctx_destroy (context);
end.

