program wuproxy;
//
//  Weather proxy device
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

  //  This is where the weather server sits
  frontend := Context.Socket( stXSub );
  frontend.connect( 'tcp://192.168.55.210:5556' );

  //  This is our public endpoint for subscribers
  backend := Context.Socket( stXPub );
  backend.bind( 'tcp://10.1.1.0:8100' );

  //  Run the proxy until the user interrupts us
  ZMQProxy( frontend, backend, nil );

  frontend.Free;
  backend.Free;
  context.Free;
end.
