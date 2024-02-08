{
  Weather proxy device
  Same as request-reply broker but using shared queue proxy
  @author cpicanco <cpicanco@ufpa.br>
}
program wuproxy;

{$mode objfpc}{$H+}

uses zmq;

var
  context, frontend, backend: Pointer;
begin
  context := zmq_ctx_new;

  //  This is where the weather server sits
  frontend := zmq_socket(context, ZMQ_XSUB);
  zmq_connect(frontend, 'tcp://192.168.55.210:5556');

  //  This is our public endpoint for subscribers
  backend := zmq_socket(context, ZMQ_XPUB);
  zmq_bind(backend, 'tcp://10.1.1.0:8100');

  //  Run the proxy until the user interrupts us
  zmq_proxy(frontend, backend, nil);

  zmq_close(frontend);
  zmq_close(backend);
  zmq_ctx_destroy (context);
end.

