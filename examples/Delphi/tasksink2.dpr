program tasksink2;
//
//  Task sink - design 2
//  Adds pub-sub flow to send kill signal to workers
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , Windows
  , zmqapi
  ;

const
  task_count = 100;
var
  context: TZMQContext;
  receiver,
  controller: TZMQSocket;
  s: Utf8String;

  task_nbr: Integer;
  fFrequency,
  fstart,
  fStop : Int64;
begin
  //  Prepare our context and socket
  context := TZMQContext.Create;
  receiver := Context.Socket( stPull );
  receiver.bind( 'tcp://*:5558' );

  //  Socket for worker control
  controller := Context.Socket( stPub );
  controller.bind( 'tcp://*:5559' );

  //  Wait for start of batch
  receiver.recv( s );

  //  Start our clock now
  QueryPerformanceFrequency( fFrequency );
  QueryPerformanceCounter( fStart );

  //  Process 100 confirmations
  for task_nbr := 0 to task_count - 1 do
  begin
    receiver.recv( s );
    if ((task_nbr / 10) * 10 = task_nbr) then
      Write( ':' )
    else
      Write( '.' );
  end;
  //  Calculate and report duration of batch
  QueryPerformanceCounter( fStop );
  Writeln( Format( 'Total elapsed time: %d msec', [
    ((MSecsPerSec * (fStop - fStart)) div fFrequency) ]) );

  controller.send( 'KILL' );

  //  Finished
  sleep(1000); //  Give 0MQ time to deliver

  receiver.Free;
  controller.Free;
  context.Free;
end.
