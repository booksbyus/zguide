with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;
with Ada.Text_IO; use Ada.Text_IO;

procedure ZMQ.examples.Server is
   ctx   : ZMQ.Contexts.Context;
   s     : ZMQ.Sockets.Socket;
   resultset_string : constant String := "OK";
begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   ctx.Initialize (1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   s.Initialize (ctx, ZMQ.Sockets.REP);

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   s.Bind ("tcp://lo:5555");

   loop
      declare
         query : ZMQ.Messages.Message;
      begin
         query.Initialize;
         --  Receive a message, blocks until one is available
         s.recv (query);
         --  Process the query
         Put_Line (query.getData);
         declare
            --  Allocate a response message and fill in an example response
            resultset : ZMQ.Messages.Message;
         begin
            resultset.Initialize (query.getData & "->" & resultset_string);
            --   Send back our canned response
            s.Send (resultset);
            resultset.Finalize;
         end;
         query.Finalize;
      end;

   end loop;
end ZMQ.Examples.Server;
