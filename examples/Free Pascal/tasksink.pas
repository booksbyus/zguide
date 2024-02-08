{
  Task sink
  Binds PULL socket to tcp://localhost:5558
  Collects results from workers via that socket
  @author cpicanco <cpicanco@ufpa.br>
}
program tasksink;

{$MODE objfpc}{$H+}

uses SysUtils, zmq, zmq.helpers, zmq.types;

var
  context, receiver: Pointer;
  LString: string;
  start_time: LongWord;
  task_nbr: Integer;

begin
  //  Prepare our context and socket
  context := zmq_ctx_new;
  receiver := zmq_socket(context, ZMQ_PULL);
  zmq_bind(receiver, 'tcp://*:5558');

  //  Wait for start of batch
  LString := s_recv(receiver);
  LString := '';

  //  Start our clock now
  start_time := GettickCount64;

  //  Process 100 confirmations
  for task_nbr := 0 to 100 -1 do
  begin
    LString := s_recv(receiver);
    LString := '';
    if ((task_nbr / 10) * 10 = task_nbr) then
      Write(':')
    else
      Write('.');
    // Flush(output);
  end;

  //  Calculate and report duration of batch
  WriteLn(Format('Total elapsed time: %d msec',
    [GettickCount64 - start_time]));

  zmq_close(receiver);
  zmq_ctx_destroy(context);
end.

