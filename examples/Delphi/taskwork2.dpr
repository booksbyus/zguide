program taskwork2;
//
//  Task worker - design 2
//  Adds pub-sub flow to receive and respond to kill signal
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
  s: String;
  poller: TZMQPoller;
  pr: TZMQPollResult;
  i,pc: Integer;
  b: Boolean;
begin
  b := True;
  context := TZMQContext.Create;

  //  Socket to receive messages on
  receiver := Context.Socket( stPull );
  receiver.connect( 'tcp://localhost:5557' );

  //  Socket to send messages to
  sender := Context.Socket( stPush );
  sender.connect( 'tcp://localhost:5558' );

  controller := Context.Socket( stSub );
  controller.connect( 'tcp://localhost:5559' );
  controller.subscribe('');

  poller := TZMQPoller.Create;
  poller.regist( receiver, [pePollIn] );
  poller.regist( controller, [pePollIn] );

  //  Process tasks forever
  while b do
  begin
    pc := poller.poll;
    for i := 0 to pc - 1 do
    begin
      pr := poller.pollResult[i];
      if pr.socket = receiver then
      begin
        receiver.recv( s );
        //  Simple progress indicator for the viewer
        Writeln( s );

        //  Do the work
        sleep( StrToInt( s ) );

        //  Send results to sink
        sender.send('');

      end else
      if pr.socket = controller then
        b := False;
    end;
  end;
  receiver.Free;
  sender.Free;
  controller.Free;
  poller.Free;
  context.Free;
end.
