program mtrelay;
//
//  Multithreaded relay
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

procedure step1( lcontext: TZMQContext );
var
  xmitter: TZMQSocket;
begin
  //  Connect to step2 and tell it we're ready
  xmitter := lContext.Socket( stPair );
  xmitter.connect( 'inproc://step2' );
  Writeln( 'Step 1 ready, signaling step 2' );
  xmitter.send( 'READY' );
  xmitter.Free;
end;

procedure step2( lcontext: TZMQContext );
var
  receiver,
  xmitter: TZMQSocket;
  s: Utf8String;
  tid: Cardinal;
begin
  //  Bind inproc socket before starting step1
  receiver := lContext.Socket( stPair );
  receiver.bind( 'inproc://step2' );
  BeginThread( nil, 0, @step1, lcontext, 0, tid );

  //  Wait for signal and pass it on
  receiver.recv( s );
  receiver.Free;

  //  Connect to step3 and tell it we're ready
  xmitter := lContext.Socket( stPair );
  xmitter.connect( 'inproc://step3' );
  Writeln( 'Step 2 ready, signaling step 3' );
  xmitter.send( 'READY' );
  xmitter.Free;
end;

var
  context: TZMQContext;
  receiver: TZMQSocket;
  tid: Cardinal;
  s: Utf8String;
begin
  context := TZMQContext.Create;

  //  Bind inproc socket before starting step2
  receiver := Context.Socket( stPair );
  receiver.bind( 'inproc://step3' );
  BeginThread( nil, 0, @step2, context, 0, tid );

  //  Wait for signal
  receiver.recv ( s );
  receiver.Free;

  Writeln( 'Test successful!' );
  context.Free;
end.
