program interrupt;
//
//  Shows how to handle Ctrl-C
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  context: TZMQContext;
  socket: TZMQSocket;
  frame: TZMQFrame;
begin
  context := TZMQContext.Create;
  socket := Context.Socket( stRep );
  socket.bind( 'tcp://*:5555' );
  while not context.Terminated do
  begin
    frame := TZMQFrame.Create;
    try
      socket.recv( frame );
    except
      on e: Exception do
        Writeln( 'Exception, ' + e.Message );
    end;
    FreeAndNil( frame );
    if socket.context.Terminated then
    begin
      Writeln( 'W: interrupt received, killing server...');
      break;
    end;
  end;
  socket.Free;
  context.Free;
end.
