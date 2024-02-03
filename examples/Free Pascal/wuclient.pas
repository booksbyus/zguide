{
  Weather update client
  Connects SUB socket to tcp://localhost:5556
  Collects weather updates and finds avg temp in zipcode
  @author cpicanco <cpicanco@ufpa.br>
}
program wuclient;

{$MODE objfpc}{$H+}{$COPERATORS ON}

uses SysUtils, zmq, zmq.helpers;

var
  context : Pointer;
  subscriber : Pointer;
  rc: Integer;
  filter : string;

  update_nbr : Integer;
  total_temp : LongInt = 0;
  total_humid : LongInt = 0;

  LString : shortstring;
  zipcode, temperature, relhumidity : integer;
begin
  //  Socket to talk to server
  WriteLn('Collecting updates from weather server…');
  context := zmq_ctx_new;
  subscriber := zmq_socket(context, ZMQ_SUB);
  rc := zmq_connect(subscriber, 'tcp://localhost:5556');
  Assert(rc = 0);

  //  Subscribe to zipcode, default is NYC, 10001
  if ParamCount > 1 then filter := ParamStr(1) else filter := '10001';
  rc := zmq_setsockopt(subscriber, ZMQ_SUBSCRIBE, @filter[1], Length(filter));
  Assert(rc = 0);

  //  Process 100 updates
  for update_nbr := 0 to 100 -1 do
    begin
      LString := s_recv(subscriber);
      ReadStr(LString, zipcode, temperature, relhumidity);
      total_temp += temperature;
      total_humid += relhumidity;
      LString := '';
    end;
  update_nbr += 1;

  WriteLn(Format('Average temperature for zipcode %s was %.2f',
    [filter, total_temp/update_nbr]));

  WriteLn(Format('Average relative humidity for zipcode %s was %.2f',
    [filter, total_humid/update_nbr]));

  zmq_close(subscriber);
  zmq_ctx_destroy(context);
end.

