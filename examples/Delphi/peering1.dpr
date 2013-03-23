program peering1;
//
//  Broker peering simulation (part 1)
//  Prototypes the state flow
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  self,
  peer: Utf8String;
  ctx: TZMQContext;
  statebe,
  statefe: TZMQSocket;
  i, rc: Integer;
  poller: TZMQPoller;
  peer_name,
  available: Utf8String;

begin
  //  First argument is this broker's name
  //  Other arguments are our peers' names
  //
  if ParamCount < 2 then
  begin
    Writeln( 'syntax: peering1 me {you}...' );
    Halt( 1 );
  end;

  self := ParamStr( 1 );
  Writeln( Format( 'I: preparing broker at %s...', [self]) );
  Randomize;

  ctx := TZMQContext.create;

  //  Bind state backend to endpoint
  statebe := ctx.Socket( stPub );
  {$ifdef unix}
  statebe.bind( Format( 'ipc://%s-state.ipc', [self] ) );
  {$else}
  statebe.bind( Format( 'tcp://127.0.0.1:%s', [self] ) );
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
    statefe.connect( Format( 'tcp://127.0.0.1:%s', [peer] ) );
    {$endif}
  end;

  //  The main loop sends out status messages to peers, and collects
  //  status messages back from peers. The zmq_poll timeout defines
  //  our own heartbeat:
  while not ctx.Terminated do
  begin
    //  Poll for activity, or 1 second timeout
    poller := TZMQPoller.Create( true );
    poller.Register( statefe, [pePollIn] );
    rc := poller.poll( 1000 );

    //  Handle incoming status messages
    if pePollIn in poller.PollItem[0].revents then
    //if pePollIn in poller.PollItem[0].events then
    begin
      statefe.recv( peer_name );
      statefe.recv( available );
      Writeln( Format( '%s - %s workers free', [ peer_name, available] ) );
    end else
      statebe.send( [self, IntToStr( Random( 10 ) ) ] );
  end;
  ctx.Free;
end.
