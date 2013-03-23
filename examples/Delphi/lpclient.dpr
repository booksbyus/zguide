program lpclient;
//
//  Lazy Pirate client
//  Use zmq_poll to do a safe request-reply
//  To run, start lpserver and then randomly kill/restart it
//  @author Varga Balazs <bb.varga@gmail.com>
//
{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

const
  REQUEST_TIMEOUT = 2500;    //  msecs, (> 1000!)
  REQUEST_RETRIES = 3;       //  Before we abandon
  SERVER_ENDPOINT = 'tcp://localhost:5555';

var
  ctx: TZMQContext;
  client: TZMQSocket;
  sequence,
  retries_left,
  expect_reply: Integer;
  request,
  reply: Utf8String;
  poller: TZMQPoller;
begin
  ctx := TZMQContext.create;
  Writeln( 'I: connecting to server...' );
  client := ctx.Socket( stReq );
  client.Linger := 0;
  client.connect( SERVER_ENDPOINT );

  poller := TZMQPoller.Create( true );
  poller.Register( client, [pePollIn] );

  sequence := 0;
  retries_left := REQUEST_RETRIES;
  while ( retries_left > 0 ) and not ctx.Terminated do
  try
    //  We send a request, then we work to get a reply
    inc( sequence );
    request := Format( '%d', [sequence] );
    client.send( request );

    expect_reply := 1;
    while ( expect_reply > 0 ) do
    begin
      //  Poll socket for a reply, with timeout
      poller.poll( REQUEST_TIMEOUT );

      //  Here we process a server reply and exit our loop if the
      //  reply is valid. If we didn't a reply we close the client
      //  socket and resend the request. We try a number of times
      //  before finally abandoning:

      if pePollIn in poller.PollItem[0].revents then
      begin
        //  We got a reply from the server, must match sequence
        client.recv( reply );
        if StrToInt( reply ) = sequence then
        begin
          Writeln( Format( 'I: server replied OK (%s)', [reply] ) );
          retries_left := REQUEST_RETRIES;
          expect_reply := 0;
        end else
          Writeln( Format( 'E: malformed reply from server: %s', [ reply ] ) );

      end else
      begin
        dec( retries_left );

        if retries_left = 0 then
        begin
          Writeln( 'E: server seems to be offline, abandoning' );
          break;
        end else
        begin
          Writeln( 'W: no response from server, retrying...' );
          //  Old socket is confused; close it and open a new one
          poller.Deregister( client, [pePollIn] );
          client.Free;
          Writeln( 'I: reconnecting to server...' );
          client := ctx.Socket( stReq );
          client.Linger := 0;
          client.connect( SERVER_ENDPOINT );
          poller.Register( client, [pePollIn] );
          //  Send request again, on new socket
          client.send( request );
        end;
      end;
    end;
  except
  end;
  poller.Free;
  ctx.Free;
end.
