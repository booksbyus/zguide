program mspoller;
//
//  Reading from multiple sockets
//  This version uses zmq_poll()
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  context: TZMQContext;
  receiver,
  subscriber: TZMQSocket;
  i,pc: Integer;
  task: TZMQFrame;
  poller: TZMQPoller;
  pollResult: TZMQPollItem;
begin
  //  Prepare our context and sockets
  context := TZMQContext.Create;

  //  Connect to task ventilator
  receiver := Context.Socket( stPull );
  receiver.connect( 'tcp://localhost:5557' );

  //  Connect to weather server
  subscriber := Context.Socket( stSub );
  subscriber.connect( 'tcp://localhost:5556' );
  subscriber.subscribe( '10001' );

  //  Initialize poll set
  poller := TZMQPoller.Create( true );
  poller.Register( receiver, [pePollIn] );
  poller.Register( subscriber, [pePollIn] );

  task := nil;

  //  Process messages from both sockets
  while True do
  begin
    pc := poller.poll;
    if pePollIn in poller.PollItem[0].revents then
    begin
      receiver.recv( task );
      // Process task
      FreeAndNil( task );
    end;
    if pePollIn in poller.PollItem[1].revents then
    begin
      subscriber.recv( task );
      // Process task
      FreeAndNil( task );
    end;
  end;
  //  We never get here
  poller.Free;
  receiver.Free;
  subscriber.Free;
  context.Free;
end.
