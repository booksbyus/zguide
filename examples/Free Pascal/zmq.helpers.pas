{
  fpc-zmq
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU Lesser General Public License (LGPL v3.0).

  You should have received a copy of the GNU Lesser General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit zmq.helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zmq.types;

  // Receive 0MQ string from socket and convert into ShortString
  // Caller must free returned string. Returns NULL if the context
  // is being terminated.
  function RecvShortString(Socket: Pointer): String;

  // Convert Shortstring to 0MQ string and send to socket
  function SendString(Socket: Pointer; const AString: String): integer;

  // Sends string as 0MQ string, as multipart non-terminal
  function SendMoreString(Socket: Pointer; const AString: String): integer;

  // Receives all message parts from socket, prints neatly
  procedure Dump(Socket: Pointer);


  procedure RecvMultiPartString(Socket: Pointer; AStringList : TStringList); inline;
  procedure SendMultiPartString(Socket: Pointer; constref AMultiPart : array of string); overload; inline;
  procedure SendMultiPartString(Socket: Pointer; const AStringList : TStringList); overload; inline;

{$IFDEF Win32}
  //  Set simple random printable identity on socket
  procedure SetID(Socket : Pointer; id : PtrInt);
{$ELSE}
  //  Set simple random printable identity on socket
  //  Caution:
  //    DO NOT call this version of s_set_id from multiple threads on MS Windows
  //    since s_set_id will call rand() on MS Windows. rand(), however, is not
  //    reentrant or thread-safe. See issue #521.
  procedure SetID(Socket: Pointer);
{$ENDIF}


var
  RandOf : TRandOf; // Provide random number from 0..(num-1)

  // Receive 0MQ string from socket and convert into ShortString
  // Caller must free returned string. Returns NULL if the context
  // is being terminated.
  s_recv : TZMQRecvStringFunction;
  s_send : TZMQSendStringFunction; // Convert Shortstring to 0MQ string and send to socket
  s_sendmore : TZMQSendStringFunction; // Sends string as 0MQ string, as multipart non-terminal
  s_dump : TZMQSocketProcedure; // Receives all message parts from socket, prints neatly
{$IFDEF Win32}
  s_set_id : TZMQSetIDProcedure; //  Set simple random printable identity on socket
{$ELSE}
  s_set_id : TZMQSocketProcedure; //  Set simple random printable identity on socket
{$ENDIF}


implementation

uses zmq, LazUTF8;

function RecvShortString(Socket: Pointer): String;
var
  buffer : array [0..High(ShortString)] of Byte;
  size : integer;
begin
  Result := '';
  size := zmq_recv(Socket, @buffer, High(buffer), 0);
  if size = -1 then exit;
  buffer[size] := $00;
  SetString(Result, PAnsiChar(@buffer), size);
end;

procedure RecvMultiPartString(Socket: Pointer; AStringList : TStringList);
var
  zmq_message : zmq_msg_t;

  procedure RecvMessage; inline;
  var message : string = '';
  begin
    zmq_msg_init(zmq_message);
    zmq_msg_recv(zmq_message, Socket, 0);
    SetString(message, PAnsiChar(zmq_msg_data(zmq_message)), zmq_msg_size(zmq_message));
    AStringList.Append(message);
  end;
begin
  zmq_message := Default(zmq_msg_t);
  AStringList.BeginUpdate;
  RecvMessage;
  while zmq_msg_more(zmq_message) = 1 do
  begin
    zmq_msg_close(zmq_message);
    RecvMessage;
  end;
  zmq_msg_close(zmq_message);
  AStringList.EndUpdate;
end;

procedure SendMultiPartString(Socket: Pointer; constref AMultiPart: array of string);
var
  i: Integer;
begin
  case Length(AMultiPart) of
    0 : zmq_send(Socket, nil, 0, 0);
    1 : zmq_send(Socket, @AMultiPart[0][1],Length(AMultiPart[0]), 0);
    else
      begin
        for i := Low(AMultiPart) to High(AMultiPart)-1 do
          SendMoreString(Socket, AMultiPart[i]);
        SendString(Socket, AMultiPart[i+1]);
      end;
  end;
end;

procedure SendMultiPartString(Socket: Pointer; const AStringList: TStringList);
var
  i: Integer;
begin
  case AStringList.Count of
    0 : zmq_send(Socket, nil, 0, 0);
    1 : zmq_send(Socket, @AStringList[0][1],Length(AStringList[0]), 0);
    else
      begin
        for i := 0 to AStringList.Count -2 do
          SendMoreString(Socket, AStringList[i]);
        SendString(Socket, AStringList[i+1]);
      end;
  end;
end;

function SendString(Socket: Pointer; const AString: String): integer;
begin
  if AString = '' then
    Result := zmq_send(Socket, nil, 0, 0)
  else
    Result := zmq_send(Socket, @AString[1], Length(AString), 0);
end;

function SendMoreString(Socket: Pointer; const  AString: String): integer;
begin
  if AString = '' then
    Result := zmq_send(Socket, nil, 0, ZMQ_SNDMORE)
  else
    Result := zmq_send(Socket, @AString[1], Length(AString), ZMQ_SNDMORE);
end;

{$IFDEF Win32}
//  Set simple random printable identity on socket
procedure SetID(Socket : Pointer; id : PtrInt);
var identity : string[10];
begin
  WriteStr(identity,Format('%04X',[Integer(id)]));
  zmq_setsockopt(socket, ZMQ_IDENTITY, @identity[1], Length(identity));
end;
{$ELSE}
procedure SetID(Socket: Pointer);
var identity : string[10];
begin
  WriteStr(identity,Format('%04X-%04X',[Random($10000), Random($10000)]));
  zmq_setsockopt(socket, ZMQ_IDENTITY, @identity[1], Length(identity));
end;
{$ENDIF}

procedure Dump(Socket: Pointer);
var
  rc : integer = -1;
  message : zmq_msg_t;

  size : integer = 0;
  data : PChar;

  is_text : Boolean;
  char_nbr : integer;
begin
  rc := zmq_msg_init(message);
  Assert(rc = 0);

  WriteLn('----------------------------------------');
  //  Process all parts of the message

  repeat
    size := zmq_msg_recv(message, Socket, 0);
    Assert(size >= 0);

    //  Dump the message as text or binary
    data := zmq_msg_data(message);
    Assert(data <> nil);
    is_text := True;
    for char_nbr := 0 to size -1 do
      if   (Ord(data[char_nbr]) < 32)
        or (Ord(data[char_nbr]) > 126)
      then is_text := False;

    Write(Format('[%.3d]',[size]),#32);
    for char_nbr := 0 to size -1 do
      if is_text then
        Write(data[char_nbr])
      else
        Write(Format('%x',[Byte(data[char_nbr])]),#32);

    Write(LineEnding);
  until zmq_msg_more(message) = 0;

  rc := zmq_msg_close(message);
  Assert(rc = 0);
end;

initialization
  RandOf := @Random;
  s_recv := @RecvShortString;
  s_send := @SendString;
  s_sendmore := @SendMoreString;
  s_dump := @Dump;
  s_set_id := @SetID;

finalization

end.
