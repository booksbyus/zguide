program psenvsub;
//
//  Pubsub envelope subscriber
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  context: TZMQContext;
  subscriber: TZMQSocket;
  address, content: Utf8String;
begin
  //  Prepare our context and subscriber
  context := TZMQContext.create;
  subscriber := context.Socket( stSub );
  subscriber.connect( 'tcp://localhost:5563' );
  subscriber.Subscribe( 'B' );
  while true do
  begin
    subscriber.recv( address );
    subscriber.recv( content );
    Writeln( Format( '[%s] %s', [address, content] ) );
  end;
  subscriber.Free;
  context.Free;
end.
