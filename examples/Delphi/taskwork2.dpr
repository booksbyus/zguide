program taskwork2;
//
//  Task worker - design 2
//  Adds pub-sub flow to receive and respond to kill signal
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
  sender,
  controller: TZMQSocket;
  frame: TZMQFrame;
  poller: TZMQPoller;
begin
  context := TZMQContext.Create;

  //  Socket to receive messages on
  receiver := Context.Socket( stPull );
  receiver.connect( 'tcp://localhost:5557' );

  //  Socket to send messages to
  sender := Context.Socket( stPush );
  sender.connect( 'tcp://localhost:5558' );

  //  Socket for control input
  controller := Context.Socket( stSub );
  controller.connect( 'tcp://localhost:5559' );
  controller.subscribe('');

  //  Process messages from receiver and controller
  poller := TZMQPoller.Create( true );
  poller.register( receiver, [pePollIn] );
  poller.register( controller, [pePollIn] );


   //  Process messages from both sockets
  while true do
  begin
    poller.poll;
    if pePollIn in poller.PollItem[0].revents then
    begin
      frame := TZMQFrame.create;
      receiver.recv( frame );

      //  Do the work
      sleep( StrToInt( frame.asUtf8String ) );
      frame.Free;

      //  Send results to sink
      sender.send('');

      //  Simple progress indicator for the viewer
      writeln('.');
    end;

    //  Any waiting controller command acts as 'KILL'
    if pePollIn in poller.PollItem[1].revents then
      break; //  Exit loop

  end;
  receiver.Free;
  sender.Free;
  controller.Free;
  poller.Free;
  context.Free;
end.
