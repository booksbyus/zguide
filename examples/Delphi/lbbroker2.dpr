program lbbroker2;
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
  WORKER_READY = '\001';      //  Signals worker is ready

//  Basic request-reply client using REQ socket
procedure client_task( args: Pointer );
var
  context: TZMQContext;
  client: TZMQSocket;
  reply: Utf8String;
begin
  context := TZMQContext.create;
  client := context.Socket( stReq );
  {$ifdef unix}
  client.connect( 'ipc://frontend.ipc' );
  {$else}
  client.connect( 'tcp://127.0.0.1:5555' );
  {$endif}

  //  Send request, get reply
  while not context.Terminated do
  try
    client.send( 'HELLO' );
    client.recv( reply );
    zNote( Format('Client: %s',[reply]) );
    sleep( 1000 );
  except
    context.Terminate;
  end;
  context.Free;
end;

//  Worker using REQ socket to do load-balancing
procedure worker_task( args: Pointer );
var
  context: TZMQContext;
  worker: TZMQSocket;
  msg: TZMQMsg;
begin
  context := TZMQContext.create;
  worker := context.Socket( stReq );
  {$ifdef unix}
  worker.connect( 'ipc://backend.ipc' );
  {$else}
  worker.connect( 'tcp://127.0.0.1:5556' );
  {$endif}
  msg := nil;

  //  Tell broker we're ready for work
  worker.send( WORKER_READY );

  //  Process messages as they arrive
  while not context.Terminated do
  try
    worker.recv( msg );
    msg.last.asUtf8String := 'OK';
    worker.send( msg );
  except
    context.Terminate;
  end;
  context.Free;
end;

var
  context: TZMQContext;
  frontend,
  backend: TZMQSocket;
  i,
  poll_c: Integer;
  tid: Cardinal;
  poller: TZMQPoller;

  workers,
  msg: TZMQMsg;

begin
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

  for i := 0 to NBR_WORKERS - 1 do
    BeginThread( nil, 0, @worker_task, nil, 0, tid );

  //  Queue of available workers
  workers := TZMQMsg.Create;
  msg := nil;
  
  poller := TZMQPoller.Create( true );
  poller.register( backend, [pePollIn] );
  poller.register( frontend, [pePollIn] );
  while not context.Terminated do
  try

     //  Poll frontend only if we have available workers
    if workers.size > 0 then
      poll_c := -1
    else
      poll_c := 1;
    poller.poll( -1, poll_c );

    //  Handle worker activity on backend
    if pePollIn in poller.PollItem[0].revents then
    begin
      //  Use worker identity for load-balancing
      backend.recv( msg );
      workers.add( msg.unwrap );

      //  Forward message to client if it's not a READY
      if msg.first.asUtf8String <> WORKER_READY then
        frontend.send( msg )
      else
        FreeAndNil( msg );
    end;

    if ( poll_c = -1 ) and ( pePollIn in poller.PollItem[1].revents ) then
    begin
      //  Get client request, route to first available worker
      frontend.recv( msg );

      msg.wrap( workers.pop );
      backend.send( msg );
    end;
    
  except
    context.Terminate;
  end;

  poller.Free;
  frontend.Free;
  backend.Free;
  context.Free;
end.

