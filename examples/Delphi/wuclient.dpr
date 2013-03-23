program wuclient;
//
//  Weather update client
//  Connects SUB socket to tcp://localhost:5556
//  Collects weather updates and finds avg temp in zipcode
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , Classes
  , zmqapi
  ;

const
  update_count = 100;

var
  context: TZMQContext;
  subscriber: TZMQSocket;
  filter: String;
  i, total_temp: Integer;
  s: Utf8String;
  tsl: TStringList;
begin
  context := TZMQContext.Create;

  //  Socket to talk to server
  Writeln ( 'Collecting updates from weather server...' );
  subscriber := Context.Socket( stSub );
  subscriber.connect( 'tcp://localhost:5556' );

  //  Subscribe to zipcode, default is NYC, 10001
  if ParamCount > 0 then
    filter := ParamStr( 1 )
  else
    filter := '10001';

  subscriber.subscribe( filter );
  tsl := TStringList.Create;
  tsl.Delimiter := ' ';

  total_temp := 0;
  //  Process 100 updates
  for i := 0 to update_count - 1 do
  begin
    subscriber.recv( s );
    tsl.Clear;
    tsl.DelimitedText := s;
    total_temp := total_temp + StrToInt( tsl[1] );
  end;
  Writeln( Format( 'Average temperature for zipcode "%s" was %fF',
        [ filter, total_temp / update_count]));

  tsl.Free;
  subscriber.Free;
  context.Free;
end.
