{
  Hello World client
  Connects REQ socket to tcp://localhost:5559
  Sends "Hello" to server, expects "World" back
  @author cpicanco <cpicanco@ufpa.br>
}
program rrclient;

{$mode objfpc}{$H+}

uses SysUtils, zmq, zmq.helpers, zmq.types;

var
  context, requester: Pointer;
  request_nbr: Integer;
  LString: string;
begin
    context := zmq_ctx_new;

    //  Socket to talk to server
    requester := zmq_socket(context, ZMQ_REQ);
    zmq_connect(requester, 'tcp://localhost:5559');
    for request_nbr := 0 to 9 do
      begin
        s_send(requester, 'Hello');
        LString := s_recv(requester);
        WriteLn(Format('Received reply %d [%s]', [request_nbr, LString]));
        LString := '';
      end;
    zmq_close(requester);
    zmq_ctx_destroy(context);
end.

