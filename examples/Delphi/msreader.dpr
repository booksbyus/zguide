program msreader;
//
//  Reading from multiple sockets
//  This version uses a simple recv loop
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
  rc: Integer;
  task,
  update: TZMQFrame;
begin
  //  Prepare our context and sockets
  context := TZMQContext.Create;

  //  Connect to task ventilator
  receiver := Context.Socket( stPull );
  receiver.RaiseEAgain := false;
  receiver.connect( 'tcp://localhost:5557' );

  //  Connect to weather server
  subscriber := Context.Socket( stSub );
  subscriber.RaiseEAgain := false;
  subscriber.connect( 'tcp://localhost:5556' );
  subscriber.subscribe( '10001' );

  //  Process messages from both sockets
  //  We prioritize traffic from the task ventilator
  while True do
  begin
    //  Process any waiting tasks
    repeat
      task := TZMQFrame.create;
      rc := receiver.recv( task, [rfDontWait] );
      if rc <> -1 then
      begin
        //  process task
      end;
      task.Free;
    until rc = -1;
    //  Process any waiting weather updates
    repeat
      update := TZMQFrame.Create;
      rc := subscriber.recv( update, [rfDontWait] );
      if rc <> -1 then
      begin
        //  process weather update
      end;
      update.Free;
    until rc = -1;
    //  No activity, so sleep for 1 msec
    sleep (1);
  end;
  //  We never get here but clean up anyhow
  receiver.Free;
  subscriber.Free;
  context.Free;
end.
