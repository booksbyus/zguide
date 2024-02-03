{
  Hello World worker
  Connects REP socket to tcp://localhost:5560
  Expects "Hello" from client, replies with "World"
  @author cpicanco <cpicanco@ufpa.br>
}
program rrworker;

{$mode objfpc}{$H+}

uses SysUtils, zmq, zmq.helpers;

var
  context, responder: Pointer;
  LString: string;
begin
  context := zmq_ctx_new;

  //  Socket to talk to clients
  responder := zmq_socket(context, ZMQ_REP);
  zmq_connect(responder, 'tcp://localhost:5560');
  repeat
    //  Wait for next request from client
    LString := s_recv(responder);
    WriteLn(Format('Received request: [%s]', [LString]));
    LString := '';

    //  Do some 'work'
    Sleep(1);

    //  Send reply back to client
    s_send(responder, 'World');
  until False;
  //  We never get here, but clean up anyhow
  zmq_close(responder);
  zmq_ctx_destroy(context);
end.

