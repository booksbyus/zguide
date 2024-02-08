{
  Task worker
  Connects PULL socket to tcp://localhost:5557
  Collects workloads from ventilator via that socket
  Connects PUSH socket to tcp://localhost:5558
  Sends results to sink via that socket
  @author cpicanco <cpicanco@ufpa.br>
}
program taskwork;

{$MODE objfpc}{$H+}

uses sysutils, zmq, zmq.helpers;

var
  context, receiver, sender: Pointer;
  LString : string;
begin
  //  Socket to receive messages on
  context := zmq_ctx_new;
  receiver := zmq_socket(context, ZMQ_PULL);
  zmq_connect(receiver, 'tcp://localhost:5557');

  //  Socket to send messages to
  sender := zmq_socket(context, ZMQ_PUSH);
  zmq_connect(sender, 'tcp://localhost:5558');

  //  Process tasks forever
  repeat
    LString := s_recv(receiver);
    Write(LString+'.');     //  Show progress
    // Flush(output);
    Sleep(StrToInt(LString));  //  Do the work
    LString := '';
    s_send(sender, '');        //  Send results to sink
  until False;

  zmq_close(receiver);
  zmq_close(sender);
  zmq_ctx_destroy(context);
end.

