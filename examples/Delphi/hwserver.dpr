program hwserver;
//
//  Hello World server
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

{$I zmq.inc}
uses
    SysUtils
  , zmq
  ;

var
  context,
  responder: Pointer;
  request,
  reply: zmq_msg_t;

begin
  context := zmq_init(1);

  //  Socket to talk to clients
  responder := zmq_socket( context, ZMQ_REP );
  zmq_bind( responder, 'tcp://*:5555' );

  while true do
  begin
    //  Wait for next request from client
    zmq_msg_init( request );
    {$ifdef zmq3}
    zmq_recvmsg( responder, request, 0 );
    {$else}
    zmq_recv( responder, request, 0 );
    {$endif}
    Writeln( 'Received Hello' );
    zmq_msg_close( request );

    //  Do some 'work'
    sleep( 1000 );

    //  Send reply back to client
    zmq_msg_init( reply );
    zmq_msg_init_size( reply, 5 );
    Move( 'World', zmq_msg_data( reply )^, 5 );
    {$ifdef zmq3}
    zmq_sendmsg( responder, reply, 0 );
    {$else}
    zmq_send( responder, reply, 0 );
    {$endif}
    zmq_msg_close( reply );

  end;

  //  We never get here but if we did, this would be how we end
  zmq_close( responder );
  zmq_term( context );
end.
