{
  Weather update server
  Binds PUB socket to tcp://*:5556
  Publishes random weather updates
  @author cpicanco <cpicanco@ufpa.br>
}
program wuserver;

{$MODE objfpc}{$H+}

uses SysUtils, zmq, zmq.helpers;

var
  context : Pointer;
  publisher : Pointer;
  rc : integer;

  zipcode, temperature, relhumidity : integer;
  update : string[20];

begin
  //  Prepare our context and publisher
  context := zmq_ctx_new;
  publisher := zmq_socket(context, ZMQ_PUB);
  rc := zmq_bind(publisher, 'tcp://*:5556');
  Assert(rc = 0);

  //  Initialize random number generator
  Randomize;
  repeat
    //  Get values that will fool the boss
    zipcode     := RandOf(100000);
    temperature := RandOf(215) - 80;
    relhumidity := RandOf(50) + 10;

    //  Send message to all subscribers
    WriteStr(update, Format('%5d %d %d', [zipcode, temperature, relhumidity]));
    s_send(publisher, update);
  until False;

  zmq_close(publisher);
  zmq_ctx_destroy(context);
end.

