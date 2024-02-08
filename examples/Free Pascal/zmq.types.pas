unit zmq.types;

{$mode objfpc}{$H+}

interface

type TRandOf = function(Number : Integer): Integer;
type TZMQRecvStringFunction = function(Socket : Pointer) : string;
type TZMQSendStringFunction = function(Socket : Pointer; const AString : String): integer;
type TZMQSocketProcedure = procedure(Socket : Pointer);

{$IFDEF Win32}
type TZMQSetIDProcedure = procedure(Socket : Pointer; id : PtrInt);
{$ENDIF}

implementation

end.

