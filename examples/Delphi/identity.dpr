program identity;
//
//  Demonstrate identities as used by the request-reply pattern.  Run this
//  program by itself.
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  , zhelpers
  ;

var
  context: TZMQContext;
  sink,
  anonymous,
  identified: TZMQSocket;
begin
  context := TZMQContext.create;

  sink := context.Socket( stRouter );
  sink.bind( 'inproc://example' );

  //  First allow 0MQ to set the identity
  anonymous := context.Socket( stReq );
  anonymous.connect( 'inproc://example' );
  anonymous.send( 'ROUTER uses a generated UUID' );
  s_dump( sink );

  //  Then set the identity ourself
  identified := context.Socket( stReq );
  identified.Identity := 'PEER2';
  identified.connect( 'inproc://example' );
  identified.send( 'ROUTER socket uses REQ''s socket identity' );
  s_dump( sink );

  sink.Free;
  anonymous.Free;
  identified.Free;
  context.Free;
end.

