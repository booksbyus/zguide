{
  Task ventilator
  Binds PUSH socket to tcp://localhost:5557
  Sends batch of tasks to workers via that socket
  @author cpicanco <cpicanco@ufpa.br>
}
program taskvent;

{$MODE objfpc}{$H+}{$COPERATORS ON}

uses SysUtils, zmq, zmq.helpers;

var
  context, sender, sink : Pointer;
  task_nbr : integer;
  total_msec : integer = 0; //  Total expected cost in msecs
  workload : integer;

  LString : string;
begin
  context := zmq_ctx_new;

  //  Socket to send messages on
  sender := zmq_socket(context, ZMQ_PUSH);
  zmq_bind(sender, 'tcp://*:5557');

  //  Socket to send start of batch message on
  sink := zmq_socket(context, ZMQ_PUSH);
  zmq_connect(sink, 'tcp://localhost:5558');

  WriteLn('Press Enter when the workers are ready: ');
  ReadLn;
  WriteLn('Sending tasks to workers…');

  //  The first message is "0" and signals start of batch
  s_send(sink, '0');

  //  Initialize random number generator
  Randomize;

  //  Send 100 tasks
  for task_nbr := 0 to 100 -1 do
    begin
      //  Random workload from 1 to 100msecs
      workload := randof(100) + 1;
      total_msec += workload;
      LString := IntToStr(workload);
      s_send(sender, LString);
    end;

  WriteLn(Format('Total expected cost: %d msec', [total_msec]));

  zmq_close(sink);
  zmq_close(sender);
  zmq_ctx_destroy(context);
end.

