with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Text_IO; use Ada.Text_IO;

procedure ZMQ.examples.Client is
   ctx              : ZMQ.Contexts.Context;
   s                : ZMQ.Sockets.Socket;
begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   ctx.Initialize (1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   s.Initialize (ctx, Sockets.REQ);

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   s.Connect ("tcp://localhost:5555");
   for i in  1 .. 10 loop
      declare
         query_string : constant String := "SELECT * FROM mytable";
         query        : ZMQ.Messages.Message;
      begin
         query.Initialize (query_string & "(" & i'Img & ");");
         s.Send (query);
         query.Finalize;
      end;

      declare
         resultset        : ZMQ.Messages.Message;
      begin
         resultset.Initialize;
         s.recv (resultset);
         Put_Line ('"' & resultset.getData & '"');
         resultset.Finalize;
      end;
   end loop;
end ZMQ.Examples.Client;
