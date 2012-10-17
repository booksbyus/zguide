program syncpub;
//
//  Synchronized publisher
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqApi
  ;

const
  SUBSCRIBERS_EXPECTED = 2;

var
  context: TZMQContext;
  publisher,
  syncservice: TZMQSocket;
  subscribers: Integer;
  str: String;
  i: Integer;
begin
  //  We wait for 10 subscribers
  context := TZMQContext.create;

  //  Socket to talk to clients
  publisher := Context.Socket( stPub );
  publisher.bind( 'tcp://*:5561' );

  //  Socket to receive signals
  syncservice := Context.Socket( stRep );
  syncservice.bind( 'tcp://*:5562' );

  //  Get synchronization from subscribers
  Writeln( 'Waiting for subscribers' );
  subscribers := 0;
  while ( subscribers < SUBSCRIBERS_EXPECTED ) do
  begin
    //  - wait for synchronization request
    syncservice.recv( str );
    //  - send synchronization reply
    syncservice.send( '' );
    Inc( subscribers );
  end;

  //  Now broadcast exactly 1M updates followed by END
  Writeln( 'Broadcasting messages' );
  for i := 0 to 1000000 - 1 do
    publisher.send( 'Rhubarb' );

  publisher.send( 'END' );

  publisher.Free;
  syncservice.Free;
  context.Free;
end.
