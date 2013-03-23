program rrserver;
//
//  Hello World server
//  Connects REP socket to tcp://*:5560
//  Expects "Hello" from client, replies with "World"
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  context: TZMQContext;
  responder: TZMQSocket;
  s: Utf8String;
begin
  context := TZMQContext.Create;

  //  Socket to talk to clients
  responder := Context.Socket( stRep );
  responder.connect( 'tcp://localhost:5560' );

  while True do
  begin
    //  Wait for next request from client
    responder.recv( s );
    Writeln( Format( 'Received request: [%s]', [ s ] ) );

    //  Do some 'work'
    sleep( 1 );

    //  Send reply back to client
    responder.send( 'World' );
  end;
  //  We never get here but clean up anyhow
  responder.Free;
  context.Free;
end.
