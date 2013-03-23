program mtserver;
//
//  Multithreaded Hello World server
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

procedure worker_routine( lcontext: TZMQContext );
var
  receiver: TZMQSocket;
  s: Utf8String;
begin
  //  Socket to talk to dispatcher
  receiver := lContext.Socket( stRep );
  receiver.connect( 'inproc://workers' );

  while True do
  begin
    receiver.recv( s );
    Writeln( Format( 'Received request: [%s]', [s] ) );
    //  Do some 'work'
    sleep (1000);
    //  Send reply back to client
    receiver.send( 'World' );
  end;
  receiver.Free;
end;

var
  context: TZMQContext;
  clients,
  workers: TZMQSocket;
  i: Integer;
  tid: Cardinal;
begin
  context := TZMQContext.Create;

  //  Socket to talk to clients
  clients := Context.Socket( stRouter );
  clients.bind( 'tcp://*:5555' );

  //  Socket to talk to workers
  workers := Context.Socket( stDealer );
  workers.bind( 'inproc://workers' );

  //  Launch pool of worker threads
  for i := 0 to 4 do
    BeginThread( nil, 0, @worker_routine, context, 0, tid );

  //  Connect work threads to client threads via a queue
  ZMQProxy( clients, workers, nil );

  //  We never get here but clean up anyhow
  clients.Free;
  workers.Free;
  context.Free;
end.
