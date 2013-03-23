program lbbroker;
//
//  Load-balancing broker
//  Clients and workers are shown here in-process
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    Windows
  , SysUtils
  , zmqapi
  , zhelpers
  ;

const
  NBR_CLIENTS = 10;
  NBR_WORKERS = 3;

//  Basic request-reply client using REQ socket
procedure client_task( args: Pointer );
var
  context: TZMQContext;
  client: TZMQSocket;
  reply: Utf8String;
begin
  context := TZMQContext.create;
  client := context.Socket( stReq );
  s_set_id( client ); //  Set a printable identity
  {$ifdef unix}
  client.connect( 'ipc://frontend.ipc' );
  {$else}
  client.connect( 'tcp://127.0.0.1:5555' );
  {$endif}

  //  Send request, get reply
  client.send( 'HELLO' );
  client.recv( reply );
  zNote( Format('Client: %s',[reply]) );
  client.Free;
  context.Free;
end;

//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//  This is the worker task, using a REQ socket to do load-balancing.
procedure worker_task( args: Pointer );
var
  context: TZMQContext;
  worker: TZMQSocket;
  identity,
  empty,
  request: Utf8String;
begin
  context := TZMQContext.create;
  worker := context.Socket( stReq );
  s_set_id( worker ); //  Set a printable identity
  {$ifdef unix}
  worker.connect( 'ipc://backend.ipc' );
  {$else}
  worker.connect( 'tcp://127.0.0.1:5556' );
  {$endif}

  //  Tell broker we're ready for work
  worker.send( 'READY' );

  while true do
  begin
    //  Read and save all frames until we get an empty frame
    //  In this example there is only 1 but it could be more
    worker.recv( identity );
    worker.recv( empty );
    Assert( empty = '' );

    //  Get request, send reply
    worker.recv( request );
    zNote( Format('Worker: %s',[request]) );

    worker.send([
      identity,
      '',
      'OK'
    ]);
  end;
  worker.Free;
  context.Free;
end;

//  This is the main task. It starts the clients and workers, and then
//  routes requests between the two layers. Workers signal READY when
//  they start; after that we treat them as ready when they reply with
//  a response back to a client. The load-balancing data structure is
// just a queue of next available workers.
var
  context: TZMQContext;
  frontend,
  backend: TZMQSocket;
  i,j,
  client_nbr,
  poll_c: Integer;
  tid: Cardinal;
  poller: TZMQPoller;

  //  Queue of available workers
  available_workers: Integer = 0;
  worker_queue: Array[0..9] of String;
  worker_id,
  empty,
  client_id,
  reply,
  request: Utf8String;
begin
  //  Prepare our context and sockets
  context := TZMQContext.create;
  frontend := context.Socket( stRouter );
  backend := context.Socket( stRouter );
  {$ifdef unix}
  frontend.bind( 'ipc://frontend.ipc' );
  backend.bind( 'ipc://backend.ipc' );
  {$else}
  frontend.bind( 'tcp://127.0.0.1:5555' );
  backend.bind( 'tcp://127.0.0.1:5556' );
  {$endif}

  for i := 0 to NBR_CLIENTS - 1 do
    BeginThread( nil, 0, @client_task, nil, 0, tid );
  client_nbr := NBR_CLIENTS;

  for i := 0 to NBR_WORKERS - 1 do
    BeginThread( nil, 0, @worker_task, nil, 0, tid );

  //  Here is the main loop for the least-recently-used queue. It has two
  //  sockets; a frontend for clients and a backend for workers. It polls
  //  the backend in all cases, and polls the frontend only when there are
  //  one or more workers ready. This is a neat way to use 0MQ's own queues
  //  to hold messages we're not ready to process yet. When we get a client
  //  reply, we pop the next available worker, and send the request to it,
  //  including the originating client identity. When a worker replies, we
  //  re-queue that worker, and we forward the reply to the original client,
  //  using the reply envelope.

  poller := TZMQPoller.Create( true );
  poller.register( backend, [pePollIn] );
  poller.register( frontend, [pePollIn] );
  while not context.Terminated and ( client_nbr > 0 ) do
  begin

     //  Poll frontend only if we have available workers
    if available_workers > 0 then
      poll_c := -1
    else
      poll_c := 1;
    poller.poll( -1, poll_c );

    //  Handle worker activity on backend
    if pePollIn in poller.PollItem[0].revents then
    begin
      //  Queue worker address for LRU routing
      backend.recv( worker_id );
      Assert( available_workers < NBR_WORKERS );
      worker_queue[available_workers] := worker_id;
      inc( available_workers );

      //  Second frame is empty
      backend.recv( empty );
      Assert( empty = '' );

      //  Third frame is READY or else a client reply address
      backend.recv( client_id );

      //  If client reply, send rest back to frontend
      if client_id <> 'READY' then
      begin
        backend.recv( empty );
        Assert( empty = '' );

        backend.recv( reply );
        frontend.send([
          client_id,
          '',
          reply
        ]);
        dec( client_nbr );
      end;

    end;

    //  Here is how we handle a client request:
    if ( poll_c = -1 ) and ( pePollIn in poller.PollItem[1].revents ) then
    begin
      //  Now get next client request, route to last-used worker
      //  Client request is [address][empty][request]
      frontend.recv( client_id );
      frontend.recv( empty );
      Assert( empty = '' );
      frontend.recv( request );
      backend.send([
        worker_queue[0],
        '',
        client_id,
        '',
        request
      ]);

      //  Dequeue and drop the next worker address
      dec( available_workers );
      for j := 0 to available_workers - 1 do
        worker_queue[j] := worker_queue[j+1];
    end;

  end;
  
  poller.Free;
  frontend.Free;
  backend.Free;
  context.Free;
end.
