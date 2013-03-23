program taskvent;
//
//  Task ventilator
//  Binds PUSH socket to tcp://localhost:5557
//  Sends batch of tasks to workers via that socket
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

const
  task_count = 100;

var
  context: TZMQContext;
  sender,
  sink: TZMQSocket;
  s: String;
  i,
  total_msec,
  workload: Integer;

begin
  context := TZMQContext.Create;

  //  Socket to send messages on
  sender := Context.Socket( stPush );
  sender.bind( 'tcp://*:5557' );

  //  Socket to send start of batch message on
  sink := Context.Socket( stPush );
  sink.connect( 'tcp://localhost:5558' );

  Write( 'Press Enter when the workers are ready: ' );
  Readln( s );
  Writeln( 'Sending tasks to workers...' );

  //  The first message is "0" and signals start of batch
  sink.send( '0' );

  //  Initialize random number generator
  randomize;

  //  Send 100 tasks
  total_msec := 0; //  Total expected cost in msecs
  for i := 0 to task_count - 1 do
  begin
    //  Random workload from 1 to 100msecs
    workload := Random( 100 ) + 1;
    total_msec := total_msec + workload;

    s := IntToStr( workload );
    sender.send( s );
  end;
  Writeln( Format( 'Total expected cost: %d msec', [total_msec] ) );
  sleep(1000); //  Give 0MQ time to deliver

  sink.Free;
  sender.Free;
  Context.Free;
end.
