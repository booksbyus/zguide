program syncsub;
//
//  Synchronized subscriber
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqApi
  ;

var
  context: TZMQContext;
  subscriber,
  syncclient: TZMQSocket;
  str: Utf8String;
  i: Integer;
begin
  context := TZMQContext.Create;

  //  First, connect our subscriber socket
  subscriber := Context.Socket( stSub );
  subscriber.RcvHWM := 1000001;
  subscriber.connect( 'tcp://localhost:5561' );
  subscriber.Subscribe( '' );

  //  0MQ is so fast, we need to wait a while...
  sleep (1000);

  //  Second, synchronize with publisher
  syncclient := Context.Socket( stReq );
  syncclient.connect( 'tcp://localhost:5562' );

  //  - send a synchronization request
  syncclient.send( '' );

  //  - wait for synchronization reply
  syncclient.recv( str );

  //  Third, get our updates and report how many we got
  i := 0;
  while True do
  begin
    subscriber.recv( str );
    if str = 'END' then
      break;
    inc( i );
  end;
  Writeln( Format( 'Received %d updates', [i] ) );

  subscriber.Free;
  syncclient.Free;
  context.Free;
end.
