program wuclient;
//
//  Weather update client
//  Connects SUB socket to tcp://localhost:5556
//  Collects weather updates and finds avg temp in zipcode
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
  update_nbr: Integer;
  total_temp: Integer = 0;

  s: String;
  tsl: TStringList;
begin
  context := TZMQContext.Create;

  //  Socket to talk to server
  Writeln ( 'Collecting updates from weather server' );
  subscriber := Context.Socket( stSub );
  subscriber.connect( 'tcp://localhost:5556' );

  filter := '10001';
  subscriber.subscribe( filter );
  tsl := TStringList.Create;
  tsl.Delimiter := ' ';

  for update_nbr := 0 to update_count - 1 do
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
