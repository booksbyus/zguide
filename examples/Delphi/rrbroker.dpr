program rrbroker;
//
//  Simple request-reply broker
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
  poller: TZMQPoller;
  msg: TZMQFrame;
  more: Boolean;

begin
  //  Prepare our context and sockets
  context := TZMQContext.Create;
  frontend := Context.Socket( stRouter );
  backend := Context.Socket( stDealer );
  frontend.bind( 'tcp://*:5559' );
  backend.bind( 'tcp://*:5560' );

  //  Initialize poll set
  poller := TZMQPoller.Create( true );
  poller.register( frontend, [pePollIn] );
  poller.register( backend, [pePollIn] );

  //  Switch messages between sockets
  while True do
  begin
    poller.poll;
    more := true;
    if pePollIn in poller.PollItem[0].revents then
    while more do
    begin
      //  Process all parts of the message
      msg := TZMQFrame.Create;
      frontend.recv( msg );
      more := frontend.rcvMore;
      if more then
        backend.send( msg, [sfSndMore] )
      else
        backend.send( msg, [] );
    end;

    if pePollIn in poller.PollItem[1].revents then
    while more do
    begin
      //  Process all parts of the message
      msg := TZMQFrame.Create;
      backend.recv( msg );
      more := backend.rcvMore;
      if more then
        frontend.send( msg, [sfSndMore] )
      else
        frontend.send( msg, [] );
    end;

  end;
  //  We never get here but clean up anyhow
  poller.Free;
  frontend.Free;
  backend.Free;
  context.Free;
end.
