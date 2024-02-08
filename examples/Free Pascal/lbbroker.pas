{
  Load-balancing broker
  Clients and workers are shown here in-process
  @author cpicanco <cpicanco@ufpa.br>
}
program lbbroker;

{$mode objfpc}{$H+}{$COPERATORS ON}

uses
  {$IFDEF UNIX}cthreads, cmem{$ENDIF}, SysUtils,
  zmq, zmq.helpers, zmq.types;

//  Dequeue operation for queue implemented as array of string
procedure DEQUEUE(var q: array of string);
  function QueueSize(S : array of string): LongWord;
  var
    i : integer;
  begin
    Result := 0;
    for i := Low(s) to High(s) do
      Result += SizeOf(S[i]);
  end;
begin
  Move(q[1], q[0], QueueSize(q) - SizeOf(q[0]));
end;

//  Basic request-reply client using REQ socket
//  Because s_send and s_recv can't handle 0MQ binary identities, we
//  set a printable text identity to allow routing.

function client_task(args: pointer):PtrInt;
var
  context, client: Pointer;
  reply : string;
begin
  context := zmq_ctx_new;
  client := zmq_socket(context, ZMQ_REQ);

{$IFDEF WIN32}
  s_set_id(client, PtrInt(args));
  zmq_connect(client, 'tcp://localhost:5672'); // frontend
{$ELSE}
  s_set_id(client); // Set a printable identity
  zmq_connect(client, 'ipc://frontend.ipc');
{$ENDIF}

  //  Send request, get reply
  s_send(client, 'HELLO');
  reply := s_recv(client);
  WriteLn('Client: '+reply);
  reply := '';
  zmq_close(client);
  zmq_ctx_destroy(context);
  Result := PtrInt(0);
end;
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//  This is the worker task, using a REQ socket to do load-balancing.
//  Because s_send and s_recv can't handle 0MQ binary identities, we
//  set a printable text identity to allow routing.

function worker_task(args: pointer): PtrInt;
var
  context, worker: Pointer;
  identity, request, empty: string;
begin
  context := zmq_ctx_new;
  worker := zmq_socket(context, ZMQ_REQ);

{$IFDEF WIN32}
  s_set_id(worker, PtrInt(args));
  zmq_connect(worker, 'tcp://localhost:5673'); // backend
{$ELSE}
  s_set_id(worker);
  zmq_connect(worker, 'ipc://backend.ipc');
{$ENDIF}

  //  Tell broker we're ready for work
  s_send(worker, 'READY');

  repeat
    //  Read and save all frames until we get an empty frame
    //  In this example there is only 1, but there could be more
    identity := s_recv(worker);
    empty := s_recv(worker);
    Assert(empty = '');
    empty := '';

    //  Get request, send reply
    request := s_recv(worker);
    WriteLn('Worker: '+request);
    request := '';

    s_sendmore(worker, identity);
    s_sendmore(worker, '');
    s_send(worker, 'OK');
    identity := '';
  until False;
  zmq_close(worker);
  zmq_ctx_destroy(context);
  Result := PtrInt(0);
end;

//  This is the main task. It starts the clients and workers, and then
//  routes requests between the two layers. Workers signal READY when
//  they start; after that we treat them as ready when they reply with
//  a response back to a client. The load-balancing data structure is
//  just a queue of next available workers.

const
  NBR_CLIENTS = 10;
  NBR_WORKERS =  3;

var
  context, frontend, backend: Pointer;
  client_nbr, worker_nbr, available_workers, rc: Integer;

  worker_queue : array of string;
  items : array [0..1] of zmq_pollitem_t;

  worker_id, empty, client_id, reply, request : string;

begin
  //  Prepare our context and sockets
  context := zmq_ctx_new;
  frontend := zmq_socket(context, ZMQ_ROUTER);
  backend := zmq_socket(context, ZMQ_ROUTER);

{$IFDEF WIN32}
  zmq_bind(frontend, 'tcp://*:5672'); // frontend
  zmq_bind(backend, 'tcp://*:5673'); // backend
{$ELSE}
  zmq_bind(frontend, 'ipc://frontend.ipc');
  zmq_bind(backend, 'ipc://backend.ipc');
{$ENDIF}

  for client_nbr := 0 to  NBR_CLIENTS -1 do
    BeginThread(@client_task, @client_nbr);

  for worker_nbr := 0 to NBR_WORKERS -1 do
    BeginThread(@worker_task, @worker_nbr);

  //  Here is the main loop for the least-recently-used queue. It has two
  //  sockets; a frontend for clients and a backend for workers. It polls
  //  the backend in all cases, and polls the frontend only when there are
  //  one or more workers ready. This is a neat way to use 0MQ's own queues
  //  to hold messages we're not ready to process yet. When we get a client
  //  reply, we pop the next available worker and send the request to it,
  //  including the originating client identity. When a worker replies, we
  //  requeue that worker and forward the reply to the original client
  //  using the reply envelope.

  //  Queue of available workers
  available_workers := 0;
  SetLength(worker_queue, 10);
  repeat
    with items[0] do
    begin
      socket := backend;
      fd := 0;
      events := ZMQ_POLLIN;
      revents := 0;
    end;
    with items[1] do
    begin
      socket := frontend;
      fd := 0;
      events := ZMQ_POLLIN;
      revents := 0;
    end;

    //  Poll frontend only if we have available workers
    if available_workers = 2 then
      rc := zmq_poll(items[0], 2, -1)
    else
      rc := zmq_poll(items[0], 1, -1);

    if rc = -1 then break; //  Interrupted

    //  Handle worker activity on backend
    if (items[0].revents and ZMQ_POLLIN) > 0 then
    begin
      //  Queue worker identity for load-balancing
      worker_id := s_recv(backend);
      Assert(available_workers < NBR_WORKERS);
      worker_queue[available_workers] := worker_id;
      available_workers += 1;

      //  Second frame is empty
      empty := s_recv(backend);
      Assert(empty = '');
      empty := '';

      //  Third frame is READY or else a client reply identity
      client_id := s_recv(backend);

      //  If client reply, send rest back to frontend
      if client_id <> 'READY' then
        begin
          empty := s_recv(backend);
          Assert(empty = '');
          empty := '';

          reply := s_recv(backend);
          s_sendmore(frontend, client_id);
          s_sendmore(frontend, '');
          s_send(frontend, reply);
          reply := '';
          client_nbr -= 1;
          if client_nbr = 0 then break; //  Exit after N messages
        end;

      client_id := '';
    end;
    //  Here is how we handle a client request:

    if (items[1].revents and ZMQ_POLLIN) > 0 then
    begin
      //  Now get next client request, route to last-used worker
      //  Client request is [identity][empty][request]
      client_id := s_recv(frontend);
      empty := s_recv(frontend);
      Assert(empty = '');
      empty := '';

      request := s_recv(frontend);

      s_sendmore(backend, worker_queue[0]);
      s_sendmore(backend, '');
      s_sendmore(backend, client_id);
      s_sendmore(backend, '');
      s_send(backend, request);

      client_id := '';
      request := '';

      //  Dequeue and drop the next worker identity
      worker_queue[0] := '';
      DEQUEUE(worker_queue);
      available_workers -= 1;
    end;
  until False;
  zmq_close(frontend);
  zmq_close(backend);
  zmq_ctx_destroy(context);
end.

