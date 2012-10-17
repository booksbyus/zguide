program wuserver;
//
//  Weather update server
//  Binds PUB socket to tcp://*:5556
//  Publishes random weather updates
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  context : TZMQContext;
  publisher : TZMQSocket;

  zipcode,
  temperature,
  relhumidity: Integer;
begin
  //  Prepare our context and publisher
  context := TZMQContext.create;
  publisher := Context.Socket( stPub );
  publisher.bind( 'tcp://*:5556' );
//  publisher.bind( 'ipc://weather.ipc' );

  Randomize;
  while True do
  begin
    zipcode := Random( 100000 );
    temperature := Random( 215 ) - 80;
    relhumidity := Random( 50 ) + 10;
    publisher.Send( Format( '%05d %d %d', [zipcode, temperature, relhumidity] ) );
  end;
  publisher.Free;
  context.Free;
end.
