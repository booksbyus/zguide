program spqueue;
//
//  Simple Pirate broker
//  This is identical to load-balancing pattern, with no reliability
//  mechanisms. It depends on the client for recovery. Runs forever.
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

const
  WORKER_READY =  '\001';      //  Signals worker is ready

var
  ctx: TZMQContext;
  frontend,
  backend: TZMQSocket;
  workers: TZMQMsg;
  poller: TZMQPoller;
  pc: Integer;
  msg: TZMQMsg;
  identity,
  frame: TZMQFrame;
begin
  ctx := TZMQContext.create;
  frontend := ctx.Socket( stRouter );
  backend := ctx.Socket( stRouter );
  frontend.bind( 'tcp://*:5555' );    //  For clients
  backend.bind( 'tcp://*:5556' );    //  For workers

  //  Queue of available workers
  workers := TZMQMsg.create;

  poller := TZMQPoller.Create( true );
  poller.Register( backend, [pePollIn] );
  poller.Register( frontend, [pePollIn] );


  //  The body of this example is exactly the same as lbbroker2.
  while not ctx.Terminated do
  try
    //  Poll frontend only if we have available workers
    if workers.size > 0 then
      pc := 2
    else
      pc := 1;
    poller.poll( 1000, pc );

    //  Handle worker activity on backend
    if pePollIn in poller.PollItem[0].revents then
    begin
      //  Use worker identity for load-balancing
      backend.recv( msg );
      identity := msg.unwrap;
      workers.add( identity );

      //  Forward message to client if it's not a READY
      frame := msg.first;
      if frame.asUtf8String = WORKER_READY then
      begin
        msg.Free;
        msg := nil;
      end else
        frontend.send( msg );
    end;

    if pePollIn in poller.PollItem[1].revents then
    begin
      //  Get client request, route to first available worker
      frontend.recv( msg );
      msg.wrap( workers.pop );
      backend.send( msg );
    end;
  except
  end;

  workers.Free;
  ctx.Free;
end.

