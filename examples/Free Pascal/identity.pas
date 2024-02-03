{
  //  Demonstrate request-reply identities
  @author cpicanco <cpicanco@ufpa.br>
}
program identity;

{$mode objfpc}{$H+}

uses SysUtils, zmq, zmq.helpers;

var
  context, sink, anonymous, identified: Pointer;
  optval : string = 'PEER2';
begin
  context := zmq_ctx_new;
  sink := zmq_socket(context, ZMQ_ROUTER);
  zmq_bind(sink, 'inproc://example');

  //  First allow 0MQ to set the identity
  anonymous := zmq_socket(context, ZMQ_REQ);
  zmq_connect(anonymous, 'inproc://example');
  s_send(anonymous, 'ROUTER uses a generated 5 byte identity');
  s_dump(sink);

  //  Then set the identity ourselves
  identified := zmq_socket(context, ZMQ_REQ);
  zmq_setsockopt(identified, ZMQ_IDENTITY, @optval[1], 5);
  zmq_connect(identified, 'inproc://example');
  s_send(identified, 'ROUTER socket uses REQ''s socket identity');
  s_dump(sink);

  zmq_close(sink);
  zmq_close(anonymous);
  zmq_close(identified);
  zmq_ctx_destroy (context);
end.
