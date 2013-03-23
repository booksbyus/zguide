program rrclient;
//
//  Hello World client
//  Connects REQ socket to tcp://localhost:5559
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
  s: Utf8String;
begin
  context := TZMQContext.Create;

  //  Socket to talk to server
  requester := Context.Socket( stReq );
  requester.connect( 'tcp://localhost:5559' );

  for i := 0 to 9 do
  begin
    requester.send( 'Hello' );
    requester.recv( s );
    Writeln( Format( 'Received reply %d [%s]',[i, s] ) );
  end;
  requester.Free;
  context.Free;
end.
