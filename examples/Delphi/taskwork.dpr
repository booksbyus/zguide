program taskwork;
//
//  Task worker
//  Connects PULL socket to tcp://localhost:5557
//  Collects workloads from ventilator via that socket
//  Connects PUSH socket to tcp://localhost:5558
//  Sends results to sink via that socket
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
  sender: TZMQSocket;
  s: Utf8String;
begin
  context := TZMQContext.Create;

  //  Socket to receive messages on
  receiver := Context.Socket( stPull );
  receiver.connect( 'tcp://localhost:5557' );

  //  Socket to send messages to
  sender := Context.Socket( stPush );
  sender.connect( 'tcp://localhost:5558' );

  //  Process tasks forever
  while True do
  begin
    receiver.recv( s );
    //  Simple progress indicator for the viewer
    Writeln( s );

    //  Do the work
    sleep( StrToInt( s ) );

    //  Send results to sink
    sender.send('');
  end;
  receiver.Free;
  sender.Free;
  context.Free;
end.
