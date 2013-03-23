program lpserver;
//
//  Lazy Pirate server
//  Binds REQ socket to tcp://*:5555
//  Like hwserver except:
//   - echoes request as-is
//   - randomly runs slowly, or exits to simulate a crash.
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  context: TZMQContext;
  server: TZMQSocket;
  cycles: Integer;
  request: Utf8String;
begin
  Randomize;

  context := TZMQContext.create;
  server := context.socket( stRep );
  server.bind( 'tcp://*:5555' );

  cycles := 0;
  while not context.Terminated do
  try
    server.recv( request );
    inc( cycles );

    //  Simulate various problems, after a few cycles
    if ( cycles > 3 ) and ( random(3) = 0) then
    begin
      Writeln( 'I: simulating a crash' );
      break;
    end else
    if ( cycles > 3 ) and ( random(3) = 0 ) then
    begin
      Writeln( 'I: simulating CPU overload' );
      sleep(2000);
    end;

    Writeln( Format( 'I: normal request (%s)', [request] ) );
    sleep (1000);              //  Do some heavy work
    server.send( request );
  except
  end;
  context.Free;
end.
