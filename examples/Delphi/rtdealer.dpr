program rtdealer;
//
//  ROUTER-to-DEALER example
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , Windows
  , zmqapi
  , zhelpers
  ;

const
  NBR_WORKERS = 10;

procedure worker_task( args: Pointer );
var
  context: TZMQContext;
  worker: TZMQSocket;
  total: Integer;
  workload,
  s: Utf8String;
begin
  context := TZMQContext.create;
  worker := context.Socket( stDealer );
  s_set_id( worker ); //  Set a printable identity
  worker.connect( 'tcp://localhost:5671' );

  total := 0;
  while true do
  begin
    //  Tell the broker we're ready for work
    worker.send( ['','Hi Boss'] );

    //  Get workload from broker, until finished
    worker.recv( s ); //  Envelope delimiter
    worker.recv( workload );
    if workload = 'Fired!' then
    begin
      zNote( Format( 'Completed: %d tasks', [total] ) );
      break;
    end;
    Inc( total );

    //  Do some random work
    sleep( random( 500 ) + 1 );
  end;
  worker.Free;
  context.Free;
end;

//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
var
  context: TZMQContext;
  broker: TZMQSocket;
  i,
  workers_fired: Integer;
  tid: Cardinal;
  identity,
  s: Utf8String;

  fFrequency,
  fstart,
  fStop,
  dt: Int64;

begin
  context := TZMQContext.create;
  broker := context.Socket( stRouter );
  broker.bind( 'tcp://*:5671' );

  Randomize;
  for i := 0 to NBR_WORKERS - 1 do
    BeginThread( nil, 0, @worker_task, nil, 0, tid );

  //  Start our clock now
  QueryPerformanceFrequency( fFrequency );
  QueryPerformanceCounter( fStart );

  //  Run for five seconds and then tell workers to end
  workers_fired := 0;
  while true do
  begin
    //  Next message gives us least recently used worker
    broker.recv( identity );
    broker.send( identity, [sfSndMore] );
    broker.recv( s ); //  Envelope delimiter
    broker.recv( s ); //  Response from worker
    broker.send( '', [sfSndMore] );

    QueryPerformanceCounter( fStop );
    dt := ( MSecsPerSec * ( fStop - fStart ) ) div fFrequency;

    if dt < 5000 then
      broker.send( 'Work harder' )
    else begin
      broker.send( 'Fired!' );
      Inc( workers_fired );
      if workers_fired = NBR_WORKERS then
        break;
    end;
  end;
  broker.Free;
  context.Free;
end.
