program tasksink;
//
//  Task sink
//  Binds PULL socket to tcp://localhost:5558
//  Collects results from workers via that socket
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
  receiver: TZMQSocket;
  s: Utf8String;

  i: Integer;
  fFrequency,
  fstart,
  fStop : Int64;
begin
  //  Prepare our context and socket
  context := TZMQContext.Create;
  receiver := Context.Socket( stPull );
  receiver.bind( 'tcp://*:5558' );

  //  Wait for start of batch
  receiver.recv( s );

  //  Start our clock now
  QueryPerformanceFrequency( fFrequency );
  QueryPerformanceCounter( fStart );

  //  Process 100 confirmations
  for i := 0 to task_count - 1 do
  begin
    receiver.recv( s );
    if ((i / 10) * 10 = i) then
      Write( ':' )
    else
      Write( '.' );
  end;
  //  Calculate and report duration of batch
  QueryPerformanceCounter( fStop );
  Writeln( Format( 'Total elapsed time: %d msec', [
    ((MSecsPerSec * (fStop - fStart)) div fFrequency) ]) );

  receiver.Free;
  context.Free;
 end.
