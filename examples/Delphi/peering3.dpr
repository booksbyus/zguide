program peering3;
//
//  Broker peering simulation (part 3)
//  Prototypes the full flow of status and tasks
//  @author Varga Balazs <bb.varga@gmail.com>
//

{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  , zhelpers
  ;

const
  NBR_CLIENTS = 10;
  NBR_WORKERS = 5;
  WORKER_READY = '\001';      //  Signals worker is ready

var
  //  Our own name; in practice this would be configured per node
  self: Utf8String;

//  This is the client task. It issues a burst of requests and then
//  sleeps for a few seconds. This simulates sporadic activity; when
//  a number of clients are active at once, the local workers should
//  be overloaded. The client uses a REQ socket for requests and also
//  pushes statistics to the monitor socket:

procedure client_task( args: Pointer; ctx: TZMQContext );
var
  client,
  monitor: TZMQSocket;
  burst,
  i: Integer;
  task_id,
  reply: Utf8String;
  poller: TZMQPoller;
begin
  client := ctx.Socket( stReq );
  {$ifdef unix}
  client.connect( Format( 'ipc://%s-localfe.ipc', [self] ) );
  {$else}
  client.connect( Format( 'tcp://127.0.0.1:%s', [self] ) );
  {$endif}

  monitor := ctx.Socket( stPush );
  {$ifdef unix}
  monitor.connect( Format( 'ipc://%s-monitor.ipc', [self] ) );
  {$else}
  monitor.connect( Format( 'tcp://127.0.0.1:4%s', [self] ) );
  {$endif}

  poller := TZMQPoller.Create( true );
  poller.Register( client, [pePollIn] );

  while not ctx.Terminated do
  try
    sleep( random( 5000 ) );
    burst := random( 15 );
    for i := 0 to burst - 1 do
    begin
      task_id := s_random( 5 );

      //  Send request with random hex ID
      client.send( task_id );

      //  Wait max ten seconds for a reply, then complain
      poller.poll( 10000 );

      if pePollIn in poller.PollItem[0].revents then
      begin
        client.recv( reply );
        //  Worker is supposed to answer us with our task id
        assert ( reply = task_id );

        monitor.send( reply );
      end else
      begin
        monitor.send( 'E: CLIENT EXIT - lost task ' + task_id );
        ctx.Terminate;
      end;
    end;
  except
  end;
end;

//  This is the worker task, which uses a REQ socket to plug into the
//  load-balancer. It's the same stub worker task you've seen in other
//  examples:

procedure worker_task( args: Pointer; ctx: TZMQContext );
var
  worker: TZMQSocket;
  msg: TZMQMsg;
begin
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
  try
    msg := TZMQMsg.Create;
    worker.recv( msg );

    //  Workers are busy for 0/1 seconds
    sleep(random (2000));
    worker.send( msg );
  except
  end;
end;

//  The main task begins by setting-up all its sockets. The local frontend
//  talks to clients, and our local backend talks to workers. The cloud
//  frontend talks to peer brokers as if they were clients, and the cloud
//  backend talks to peer brokers as if they were workers. The state
//  backend publishes regular state messages, and the state frontend
//  subscribes to all state backends to collect these messages. Finally,
//  we use a PULL monitor socket to collect printable messages from tasks:
var
  ctx: TZMQContext;
  cloudfe,
  cloudbe,
  localfe,
  localbe,
  statefe,
  statebe,
  monitor: TZMQSocket;
  i,
  timeout,
  previous,
  random_peer: Integer;
  peer: Utf8String;
  thr: TZMQThread;

  cloud_capacity: Integer;
  workers: TZMQMsg;

  primary,
  secondary: TZMQPoller;

  msg: TZMQMsg;
  identity,
  frame: TZMQFrame;
  data,
  status: Utf8String;

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

  //  Prepare local frontend and backend
  localfe := ctx.Socket( stRouter );
  {$ifdef unix}
  localfe.bind( Format( 'ipc://%s-localfe.ipc', [self] ) );
  {$else}
  localfe.bind( Format( 'tcp://127.0.0.1:%s', [self] ) );
  {$endif}

  localbe := ctx.Socket( stRouter );
  {$ifdef unix}
  localbe.bind( Format( 'ipc://%s-localbe.ipc', [self] ) );
  {$else}
  localbe.bind( Format( 'tcp://127.0.0.1:1%s', [self] ) );
  {$endif}

  //  Bind cloud frontend to endpoint
  cloudfe := ctx.Socket( stRouter );
  cloudfe.Identity := self;
  {$ifdef unix}
  cloudfe.bind( Format( 'ipc://%s-cloud.ipc', [self] ) );
  {$else}
  cloudfe.bind( Format( 'tcp://127.0.0.1:2%s', [self] ) );
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

  //  Bind state backend to endpoint
  statebe := ctx.Socket( stPub );
  {$ifdef unix}
  statebe.bind( Format( 'ipc://%s-state.ipc', [self] ) );
  {$else}
  statebe.bind( Format( 'tcp://127.0.0.1:3%s', [self] ) );
  {$endif}

  //  Connect statefe to all peers
  statefe := ctx.Socket( stSub );
  statefe.Subscribe('');

  for i := 2 to ParamCount do
  begin
    peer := ParamStr( i );
    Writeln( Format( 'I: connecting to state backend at "%s"', [peer] ) );
    {$ifdef unix}
    statefe.connect( Format( 'ipc://%s-state.ipc', [peer] ) );
    {$else}
    statefe.connect( Format( 'tcp://127.0.0.1:3%s', [peer] ) );
    {$endif}
  end;

  //  Prepare monitor socket
  monitor := ctx.Socket( stPull );
  {$ifdef unix}
  monitor.bind( Format( 'ipc://%s-monitor.ipc', [self] ) );
  {$else}
  monitor.bind( Format( 'tcp://127.0.0.1:4%s', [self] ) );
  {$endif}

  //  After binding and connecting all our sockets, we start our child
  //  tasks - workers and clients:
  for i := 0 to NBR_WORKERS - 1 do
  begin
    thr := TZMQThread.CreateDetachedProc( worker_task, nil );
    thr.FreeOnTerminate := true;
    thr.Resume;
  end;

  //  Start local clients
  for i := 0 to NBR_CLIENTS - 1 do
  begin
    thr := TZMQThread.CreateDetachedProc( client_task, nil );
    thr.FreeOnTerminate := true;
    thr.Resume;
  end;

  //  Queue of available workers
  cloud_capacity := 0;
  workers := TZMQMsg.Create;

  primary := TZMQPoller.Create( true );
  primary.Register( localbe, [pePollIn] );
  primary.Register( cloudbe, [pePollIn] );
  primary.Register( statefe, [pePollIn] );
  primary.Register( monitor, [pePollIn] );


  secondary := TZMQPoller.Create( true );
  secondary.Register( localfe, [pePollIn] );
  secondary.Register( cloudfe, [pePollIn] );

  //  The main loop has two parts. First we poll workers and our two service
  //  sockets (statefe and monitor), in any case. If we have no ready workers,
  //  there's no point in looking at incoming requests. These can remain on
  //  their internal 0MQ queues:
  while not ctx.Terminated do
  try
    //  If we have no workers ready, wait indefinitely
    if workers.size = 0 then
      timeout := -1
    else
      timeout := 1000;

    primary.poll( timeout );
    
    //  Track if capacity changes during this iteration
    previous := workers.size;

    //  Handle reply from local worker
    msg := nil;
    if pePollIn in primary.PollItem[0].revents then
    begin
      localbe.recv( msg );

      identity := msg.unwrap;
      workers.add( identity );

      //  If it's READY, don't route the message any further
      if msg.first.asUtf8String = WORKER_READY then
        FreeAndNil( msg );
    end else

    //  Or handle reply from peer broker
    if pePollIn in primary.PollItem[1].revents then
    begin
      cloudbe.recv( msg );
      //  We don't use peer broker identity for anything
      msg.unwrap.Free;
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

    //  If we have input messages on our statefe or monitor sockets we
    //  can process these immediately:

    if pePollIn in primary.PollItem[2].revents then
    begin
      statefe.recv( peer );
      statefe.recv( status );
      cloud_capacity := StrToInt( status );
    end;

    if pePollIn in primary.PollItem[3].revents then
    begin
      monitor.recv( status );
      zNote( status );
    end;

    //  Now route as many clients requests as we can handle. If we have
    //  local capacity we poll both localfe and cloudfe. If we have cloud
    //  capacity only, we poll just localfe. We route any request locally
    //  if we can, else we route to the cloud.

    while ( workers.size + cloud_capacity ) > 0 do
    begin
      if workers.size > 0 then
        secondary.poll( 0, 2 )
      else
        secondary.poll( 0, 1 );

      //msg := TZMQMsg.Create;
      if pePollIn in secondary.PollItem[0].revents then
        localfe.recv( msg ) else
      if pePollIn in secondary.PollItem[1].revents then
        cloudfe.recv( msg ) else
        break; //  No work, go back to primary

      if workers.size > 0 then
      begin
        frame := workers.pop;
        msg.wrap( frame );
        localbe.send( msg );
      end else
      begin
        random_peer := random( ParamCount - 2 ) + 2;
        identity := TZMQFrame.create;
        identity.asUtf8String := ParamStr( random_peer );
        msg.push( identity );
        cloudbe.send( msg );
      end;

    end;

    //  We broadcast capacity messages to other peers; to reduce chatter
    //  we do this only if our capacity changed.

    if workers.size <> previous then
    begin
      //  We stick our own identity onto the envelope
      //  Broadcast new capacity
      statebe.send( [self, IntToStr( workers.size ) ] );
    end;

  except
  end;

  //  When we're done, clean up properly
  while workers.size > 0 do
  begin
    frame := workers.pop;
    frame.Free;
  end;
  workers.Free;
  ctx.Free;

end.
