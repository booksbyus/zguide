program peering2;
//
//  Broker peering simulation (part 2)
//  Prototypes the request-reply flow
//  @author Varga Balázs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    Classes
  , SysUtils
  , zmqapi
  , zhelpers
  ;

const
  NBR_CLIENTS = 10;
  NBR_WORKERS = 3;
  WORKER_READY = '\001';      //  Signals worker is ready

var
  //  Our own name; in practice this would be configured per node
  self: String;

//  The client task does a request-reply dialog using a standard
//  synchronous REQ socket:
procedure client_task( args: Pointer );
var
  ctx: TZMQContext;
  client: TZMQSocket;
  reply: Utf8String;
begin
  ctx := TZMQContext.Create();
  client := ctx.Socket( stReq );
  {$ifdef unix}
  client.connect( Format( 'ipc://%s-localfe.ipc', [self] ) );
  {$else}
  client.connect( Format( 'tcp://127.0.0.1:%s', [self] ) );
  {$endif}

  while not ctx.Terminated do
  begin
    client.send( 'HELLO' );
    client.recv( reply );
    zNote( Format( 'Client: %s', [reply] ) );
    sleep( 1 );
  end;
  ctx.Free;
end;

//  The worker task plugs into the load-balancer using a REQ
//  socket:
procedure worker_task( args: Pointer );
var
  ctx: TZMQContext;
  worker: TZMQSocket;
  msg: TZMQMsg;
begin
  ctx := TZMQContext.create;
  worker := ctx.Socket( stReq );
  {$ifdef unix}
  worker.connect( Format( 'ipc://%s-localbe.ipc', [self] ) );
  {$else}
  worker.connect( Format( 'tcp://127.0.0.1:1%s', [self] ) );
  {$endif}


  //  Tell broker we're ready for work
  worker.send( WORKER_READY );

  //  Process messages as they arrive
  while not ctx.Terminated do
  begin
    msg := TZMQMsg.create;
    worker.recv( msg );

    zNote( Format( 'Worker: %s', [msg.last.dump ] ) );
    msg.last.asUtf8String := 'OK';
    worker.send( msg );
  end;
  ctx.Free;
end;

var
  ctx: TZMQContext;
  cloudfe,
  cloudbe,
  localfe,
  localbe: TZMQSocket;
  i: Integer;
  peer,
  s: String;
  tid: Cardinal;
  workers: TList;
  pollerbe,
  pollerfe: TZMQPoller;
  rc,timeout: Integer;
  msg: TZMQMsg;
  identity,
  frame: TZMQFrame;
  data: Utf8String;
  reroutable,
  random_peer: Integer;

//  The main task begins by setting-up its frontend and backend sockets
//  and then starting its client and worker tasks:
begin
  //  First argument is this broker's name
  //  Other arguments are our peers' names
  //
  if ParamCount < 2 then
  begin
    Writeln( 'syntax: peering2 me {you}...' );
    halt( 1 );
  end;

  // on windows it should be a 1024 <= number <= 9999
  self := ParamStr( 1 );
  writeln( Format( 'I: preparing broker at %s', [self] ) );
  randomize;

  ctx := TZMQContext.create;

  //  Bind cloud frontend to endpoint
  cloudfe := ctx.Socket( stRouter );
  cloudfe.Identity := self;
  {$ifdef unix}
  cloudfe.connect( Format( 'ipc://%s-cloud.ipc', [self] ) );
  {$else}
  cloudfe.connect( Format( 'tcp://127.0.0.1:2%s', [self] ) );
  {$endif}

  //  Connect cloud backend to all peers
  cloudbe := ctx.Socket( stRouter );
  cloudbe.Identity := self;

  for i := 2 to ParamCount do
  begin
    peer := ParamStr( i );
    Writeln( Format( 'I: connecting to cloud frontend at "%s"', [peer] ) );
    {$ifdef unix}
    cloudbe.connect( Format( 'ipc://%s-cloud.ipc', [peer] ) );
    {$else}
    cloudbe.connect( Format( 'tcp://127.0.0.1:2%s', [peer] ) );
    {$endif}
  end;

  //  Prepare local frontend and backend
  localfe := ctx.Socket( stRouter );
  {$ifdef unix}
  localfe.connect( Format( 'ipc://%s-localfe.ipc', [self] ) );
  {$else}
  localfe.connect( Format( 'tcp://127.0.0.1:%s', [self] ) );
  {$endif}

  localbe := ctx.Socket( stRouter );
  {$ifdef unix}
  localbe.connect( Format( 'ipc://%s-localbe.ipc', [self] ) );
  {$else}
  localbe.connect( Format( 'tcp://127.0.0.1:1%s', [self] ) );
  {$endif}

  //  Get user to tell us when we can start…
  Writeln( 'Press Enter when all brokers are started: ');
  Readln( s );

  //  Start local workers
  for i := 0 to NBR_WORKERS - 1 do
    BeginThread( nil, 0, @worker_task, nil, 0, tid );

  //  Start local clients
  for i := 0 to NBR_CLIENTS - 1 do
    BeginThread( nil, 0, @client_task, nil, 0, tid );

  //  Here we handle the request-reply flow. We're using load-balancing
  //  to poll workers at all times, and clients only when there are one or
  //  more workers available.

  //  Least recently used queue of available workers
  workers := TList.Create;

  pollerbe := TZMQPoller.Create( true );
  pollerbe.Register( localbe, [pePollIn] );
  pollerbe.Register( cloudbe, [pePollIn] );

  // I could do it with one poller too.
  pollerfe := TZMQPoller.Create( true );
  pollerfe.Register( localfe, [pePollIn] );
  pollerfe.Register( cloudfe, [pePollIn] );

  while not ctx.Terminated do
  begin
    //  First, route any waiting replies from workers

    //  If we have no workers anyhow, wait indefinitely
    if workers.count = 0 then
      timeout := -1
    else
      timeout := 1000;

    pollerbe.poll( timeout );
    msg := nil;

    //  Handle reply from local worker
    if pePollIn in  pollerbe.PollItem[0].events then
    begin
       msg := TZMQMsg.Create;
       pollerbe.PollItem[0].socket.recv( msg );
       identity := msg.unwrap;
       workers.Add( identity );
       //  If it's READY, don't route the message any further
       frame := msg.first;
       if frame.asUtf8String = WORKER_READY then
       begin
        msg.Free;
        msg := nil;
       end;

    //  Or handle reply from peer broker
    end else
    if pePollIn in  pollerbe.PollItem[1].events then
    begin

      msg := TZMQMsg.create;
      pollerbe.PollItem[1].socket.recv( msg );

      //  We don't use peer broker identity for anything
      identity := msg.unwrap;
      identity.Free;
    end;

    //  Route reply to cloud if it's addressed to a broker
    if msg <> nil then
    for i := 2 to ParamCount do
    begin
      data := msg.first.asUtf8String;
      if data = ParamStr( i ) then
        cloudfe.send( msg );
    end;

    //  Route reply to client if we still need to
    if msg <> nil then
      localfe.send( msg );

    //  Now we route as many client requests as we have worker capacity
    //  for. We may reroute requests from our local frontend, but not from //
    //  the cloud frontend. We reroute randomly now, just to test things
    //  out. In the next version we'll do this properly by calculating
    //  cloud capacity://

    while workers.Count > 0 do
    begin
      rc := pollerfe.poll( 0 );
      Assert( rc >= 0 );

      //  We'll do peer brokers first, to prevent starvation
      if pePollIn in pollerfe.PollItem[1].events then
      begin
        msg := TZMQMsg.create;
        cloudfe.recv( msg );
        reroutable := 0;
      end else
      if pePollIn in pollerfe.PollItem[0].events then
      begin
        msg := TZMQMsg.create;
        localfe.recv( msg );
        reroutable := 1;
      end else
        break; //  No work, go back to backends

      //  If reroutable, send to cloud 20% of the time
      //  Here we'd normally use cloud status information
      //
      if ( reroutable > 0 ) and ( ParamCount > 2 ) and ( Random( 5 ) = 0 ) then
      begin
        //  Route to random broker peer
        random_peer := random( ParamCount - 2 ) + 2;
        identity := TZMQFrame.create;
        identity.asUtf8String := ParamStr(  random_peer );
        msg.push( identity );
        cloudbe.send( msg );
      end else
      begin
        frame := workers[0];
        workers.Delete( 0 );
        msg.wrap( frame );
        localbe.send( msg );
      end;
    end;
  end;

  //  When we're done, clean up properly
  while workers.Count > 0 do
  begin
    frame := workers[0];
    workers.Delete( 0 );
    frame.Free;
  end;
  workers.Free;
  ctx.Free;

end.

