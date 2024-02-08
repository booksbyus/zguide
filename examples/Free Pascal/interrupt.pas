{
  Shows how to ugly handle Ctrl-C in unix systems
  @author cpicanco <cpicanco@ufpa.br>
}
program interrupt;

{$mode objfpc}{$H+}

uses
  SysUtils, initc, BaseUnix, UNIX,
  zmq, zmq.helpers;

function read_(__fd:longint; __buf:pointer; __nbytes:size_t):ssize_t;cdecl;external clib name 'read';
function write_(__fd:longint; __buf:pointer; __n:size_t):ssize_t;cdecl;external clib name 'write';

//  Signal handling
//
//  Create a self-pipe and call s_catch_signals(pipe's writefd) in your application
//  at startup, and then exit your main loop if your pipe contains any data.
//  Works especially well with zmq_poll.
const
  STDOUT_FILENO = 1;

var
  S_NOTIFY_MSG : string = #32;
  S_ERROR_MSG : string = 'Error while writing to self-pipe'+LineEnding;

  s_fd : integer;

  rc, flags , i: integer;
  pipefds : TFilDes;
  items : array [0..1] of zmq_pollitem_t;
  context, lsocket: Pointer;

  buffer1 : string[1];
  buffer2 : string[255];

procedure s_signal_handler(signal: longint; info: psiginfo; context: psigcontext); cdecl;
var
  rc : integer;
begin
  rc := write_(s_fd, @S_NOTIFY_MSG[1], Length(S_NOTIFY_MSG));
  if rc <> Length(S_NOTIFY_MSG) then
  begin
    write_(STDOUT_FILENO, @S_ERROR_MSG[1], Length(S_ERROR_MSG)-1);
    Halt(1);
  end;
end;

procedure s_catch_signals(fd : integer);
var
  action : sigactionrec;
begin
  s_fd := fd;
  action.sa_handler := @s_signal_handler;
  //  Doesn't matter if SA_RESTART set because self-pipe will wake up zmq_poll
  //  But setting to 0 will allow zmq_read to be interrupted.
  action.sa_flags := 0;
  FpsigEmptySet(action.sa_mask);
  FPSigaction(ESysEINTR, @action, nil);
  FPSigaction(SIGTERM, @action, nil);
end;
begin
  context := zmq_ctx_new;
  lsocket := zmq_socket(context, ZMQ_REP);
  zmq_bind(lsocket, 'tcp://*:5555');
  try
    rc := fppipe(pipefds);
    if rc <> 0 then
    begin
      WriteLn(StdErr, 'Creating self-pipe');
      Halt(1);
    end;
    for i := 0 to 1 do
    begin
        flags := fpfcntl(pipefds[0], F_GETFL, 0);
        if flags < 0 then
        begin
          WriteLn(StdErr, 'fcntl(F_GETFL)');
          Halt(1);
        end;
        rc := fpfcntl(pipefds[0], F_SETFL, flags or O_NONBLOCK);
        if rc <> 0 then
        begin
          WriteLn(StdErr, 'fcntl(F_SETFL)');
          Halt(1);
        end;
    end;

    s_catch_signals(pipefds[1]);

    with items[0] do
    begin
      socket := nil;
      fd := pipefds[0];
      events := ZMQ_POLLIN;
      revents := 0;
    end;

    with items[1] do
    begin
      socket := lsocket;
      fd := 0;
      events := ZMQ_POLLIN;
      revents := 0;
    end;

    repeat
      rc := zmq_poll(items[0], 2, -1);
      if rc = 0 then continue
      else if rc < 0 then
      begin
        if fpgeterrno = ESysEINTR then continue;
        WriteLn(StdErr, 'zmq_poll');
        Halt(1);
      end;

      // Signal pipe FD
      if (items[0].revents and ZMQ_POLLIN) > 0 then
      begin
        read_(pipefds[0], @buffer1[1], 1);  // clear notifying byte
        WriteLn('W: interrupt received, killing server…');
        break;
      end;

      // Read socket
      if (items[1].revents and ZMQ_POLLIN) > 0 then
      begin
        // Use non-blocking so we can continue to check self-pipe via zmq_poll
        rc := zmq_recv(lsocket, @buffer2[1], 255, ZMQ_NOBLOCK);
        if rc < 0 then
          if fpgeterrno = ESysEAGAIN then continue;
          if fpgeterrno = ESysEINTR then continue;
          WriteLn(StdErr, 'recv');
          Halt(1);
        end;
        WriteLn('W: recv');

        // Now send message back.
        // …
    until False;


  finally
    WriteLn('W: cleaning up');
    zmq_close(lsocket);
    zmq_ctx_destroy(context);
  end;
end.
