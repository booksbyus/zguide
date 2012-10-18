program rrbroker;
//
//  Simple request-reply broker
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
  msg: TZMQMessage;
  more: Boolean;
  i,pc: Integer;
  pollResult: TZMQPollResult;
  otherSocket: TZMQSocket;

begin
  //  Prepare our context and sockets
  context := TZMQContext.Create;
  frontend := Context.Socket( stRouter );
  backend := Context.Socket( stDealer );
  frontend.bind( 'tcp://*:5559' );
  backend.bind( 'tcp://*:5560' );

  //  Initialize poll set
  poller := TZMQPoller.Create;
  poller.regist( frontend, [pePollIn] );
  poller.regist( backend, [pePollIn] );

  //  Switch messages between sockets
  while True do
  begin
    pc := poller.poll;
    for i := 0 to pc - 1 do
    begin
      pollResult := poller.pollResult[i];
      more := True;
      if pollResult.socket = frontend then
        otherSocket := backend
      else
        otherSocket := frontend;

      if pePollIn in pollResult.revents then
      while more do
      begin
        //  Process all parts of the message
        msg := TZMQMessage.Create;
        pollResult.socket.recv( msg );
        more := pollResult.socket.rcvMore;
        if more then
          otherSocket.send( msg, [sfSndMore] )
        else
          otherSocket.send( msg, [] );
       msg.Free;
      end;

    end;
  end;
  //  We never get here but clean up anyhow
  poller.Free;
  frontend.Free;
  backend.Free;
  context.Free;
end.
