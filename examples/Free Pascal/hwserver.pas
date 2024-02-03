{
  Hello World server
  Binds REP socket to tcp://*:5555
  Expects "Hello" from client, replies with "World"
  @author cpicanco <cpicanco@ufpa.br>
}
program hwserver;

{$MODE objfpc}{$H+}

uses SysUtils, zmq;

var
  context, responder: Pointer;
  rc : integer = 0;
  buffer : array [0..4] of Char;
  S : string[5] = 'World'; // ansistring
begin
  //  Socket to talk to clients
  context := zmq_ctx_new;
  responder := zmq_socket(context, ZMQ_REP);
  rc := zmq_bind(responder, 'tcp://*:5555');
  Assert(rc = 0);
  WriteLn('hello world server initialized…');
  while True do
    begin
      zmq_recv(responder, @buffer, Length(buffer), 0);
      Writeln('Received ', buffer);
      Sleep(1000); //  Do some 'work'
      WriteLn('Sending '+S+'…');
      zmq_send(responder, @S[1], Length(S), 0);
    end;
end.
