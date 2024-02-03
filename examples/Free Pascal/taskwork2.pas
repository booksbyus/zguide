{
  Task sink - design 2
  Adds pub-sub flow to send kill signal to workers
  @author cpicanco <cpicanco@ufpa.br>
}
program taskwork2;

{$mode objfpc}{$H+}

uses SysUtils, zmq, zmq.helpers;

var
  context, receiver, controller: Pointer;
  LString : string;
  task_nbr: Integer;
  start_time: QWord;
begin
  //  Socket to receive messages on
  context := zmq_ctx_new;
  receiver := zmq_socket(context, ZMQ_PULL);
  zmq_bind(receiver, 'tcp://*:5558');

  //  Socket for worker control
  controller := zmq_socket(context, ZMQ_PUB);
  zmq_bind(controller, 'tcp://*:5559');

  //  Wait for start of batch
  LString := s_recv(receiver);
  LString := '';

  //  Start our clock now
  start_time := GetTickCount64;

  //  Process 100 confirmations
  task_nbr := 0;
  for task_nbr := 0 to 99 do
  begin
    LString := s_recv(receiver);
    LString := '';
    if ((task_nbr mod 10) = 0) then
      WriteLn(':')
    else
      WriteLn('.');
    //Flush(StdOut);
  end;
  WriteLn(Format('Total elapsed time: %d msec',
    [GetTickCount64 - start_time]));

  //  Send kill signal to workers
  s_send(controller, 'KILL');

  zmq_close(receiver);
  zmq_close(controller);
  zmq_ctx_destroy(context);
end.
