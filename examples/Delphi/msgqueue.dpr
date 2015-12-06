program msgqueue;
//
//  Simple message queuing broker
//  Same as request-reply broker but using shared queue proxy
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  context: TZMQContext;
  frontend,
  backend: TZMQSocket;

begin
  context := TZMQContext.Create;

  //  Socket facing clients
  frontend := Context.Socket( stRouter );
  frontend.bind( 'tcp://*:5559' );

  //  Socket facing services
  backend := Context.Socket( stDealer );
  backend.bind( 'tcp://*:5560' );

  //  Start the proxy
  ZMQProxy( frontend, backend, nil );

  //  We never get here...
  frontend.Free;
  backend.Free;
  context.Free;
end.
