program spworker;
//
//  Simple Pirate worker
//  Connects REQ socket to tcp://*:5556
//  Implements worker part of load-balancing
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  , zhelpers
  ;

const
  WORKER_READY = '\001';      //  Signals worker is ready

var
  ctx: TZMQContext;
  worker: TZMQSocket;
  identity: String;
  frame: TZMQFrame;
  cycles: Integer;
  msg: TZMQMsg;
begin
  ctx := TZMQContext.create;
  worker := ctx.Socket( stReq );

  //  Set random identity to make tracing easier
  identity := s_random( 8 );
  worker.Identity := identity;
  worker.connect( 'tcp://localhost:5556' );

  //  Tell broker we're ready for work
  Writeln( Format( 'I: (%s) worker ready', [identity] ) );
  frame := TZMQFrame.create;
  frame.asUtf8String := WORKER_READY;
  worker.send( frame );
  cycles := 0;
  while not ctx.Terminated do
  try
    worker.recv( msg );

    //  Simulate various problems, after a few cycles
    Inc( cycles );

    if ((cycles > 3) and (random(5) = 0)) then
    begin
      Writeln( Format( 'I: (%s) simulating a crash', [identity] ) );
      msg.Free;
      msg := nil;
      break;
    end else
    if ( (cycles > 3) and (random(5) = 0) ) then
    begin
      Writeln( Format( 'I: (%s) simulating CPU overload', [identity] ));
      sleep (3000);
    end;
    Writeln( Format('I: (%s) normal reply', [identity]) );
    sleep(1000);              //  Do some heavy work
    worker.send( msg );
  except
  end;
  ctx.Free;
end.
