program hwserverapi;
//
//  Hello World server in Delphi
//  Binds REP socket to tcp://*:5555
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
  socket: TZMQSocket;
  sMsg: Utf8String;

begin
  //  Prepare our context and socket
  context := TZMQContext.Create;
  socket := context.Socket( stRep );
  socket.bind( 'tcp://*:5555' );

  while true do
  begin
    //  Wait for next request from client
    socket.recv( sMsg );
    Writeln( Format( 'Received %s', [sMsg] ) );

    //  Do some 'work'
    sleep( 1 );

    //  Send reply back to client
    socket.send( 'World' );
  end;
  socket.Free;
  context.Free;
end.
