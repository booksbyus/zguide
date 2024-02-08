{
  ROUTER-to-REQ example
  @author cpicanco <cpicanco@ufpa.br>
}
program rtreq;

{$mode objfpc}{$H+}{$COPERATORS ON}

uses
  {$IFDEF UNIX}cthreads, cmem{$ENDIF}, SysUtils,
  zmq, zmq.helpers;

function worker_task(args : Pointer):PtrInt;
var
  context, worker: Pointer;
  workload : string;
  total: Integer;
begin
  context := zmq_ctx_new;
  worker := zmq_socket(context, ZMQ_REQ);
{$IFDEF Win32}
  s_set_id(worker, PrtInt(args));
{$ELSE}
  s_set_id(worker);          //  Set a printable identity.
{$ENDIF}

  zmq_connect(worker, 'tcp://localhost:5671');

  total := 0;
  repeat // WriteLn(ThreadID);
    //  Tell the broker we're ready for work
    s_send(worker, 'Hi Boss');

    //  Get workload from broker, until finished
    workload := s_recv(worker);
    if workload = 'Fired!' then
    begin
      WriteLn(Format('Completed: %d tasks', [total]));
      break;
    end;
    workload := '';
    total += 1;
    //  Do some random work
    Sleep(randof(500) + 1);
  until False;
  zmq_close(worker);
  zmq_ctx_destroy(context);
  Result := PtrInt(0);
end;

//  While this example runs in a single process, that is only to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.

const NBR_WORKERS = 10;

var
  context, broker: Pointer;
  worker_nbr, workers_fired: Integer;

  identity : string;
  end_time: LongWord;
begin
    context := zmq_ctx_new;
    broker := zmq_socket(context, ZMQ_ROUTER);

    zmq_bind(broker, 'tcp://*:5671');
    Randomize;

    for worker_nbr := 0 to NBR_WORKERS -1 do
      BeginThread(@worker_task, @worker_nbr);

    //  Run for five seconds and then tell workers to end
    end_time := GetTickCount64 + 5000;
    workers_fired := 0;
    repeat
      //  Next message gives us least recently used worker
      identity := s_recv(broker);
      s_sendmore(broker, identity);
      identity := '';
      s_recv(broker);     //  Envelope delimiter
      s_recv(broker);     //  Response from worker
      s_sendmore(broker, '');

      //  Encourage workers until it's time to fire them
      if (GetTickCount64 < end_time) then
        s_send(broker, 'Work harder')
      else
      begin
        s_send(broker, 'Fired!');
        workers_fired += 1;
        if workers_fired = NBR_WORKERS then break;
      end;
    until False;
    zmq_close(broker);
    zmq_ctx_destroy(context);
end.
