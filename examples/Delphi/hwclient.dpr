program hwclient;
//
//  Hello World client
//  Connects REQ socket to tcp://localhost:5555
//  Sends "Hello" to server, expects "World" back
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  context: TZMQContext;
  requester: TZMQSocket;
  i: Integer;
  sMsg: Utf8String;
begin
  context := TZMQContext.Create;

  //  Socket to talk to server
  Writeln('Connecting to hello world server...');
  requester := Context.Socket( stReq );
  requester.connect( 'tcp://localhost:5555' );

  for i := 0 to 9 do
  begin
    sMsg := 'Hello';
    Writeln( Format( 'Sending %s %d',[ sMsg, i ] ));
    requester.send( sMsg );
    requester.recv( sMsg );
    Writeln( Format( 'Received %s %d', [ sMsg, i ] ) );
  end;

  sleep(2000);
  requester.Free;
  context.Free;
end.
