with ZMQ.Sockets;
with ZMQ.Contexts;
with Ada.Text_IO;
procedure ZMQ.examples.Display is
   use Ada.Text_IO;
   Context  : aliased Contexts.Context;
   Socket   : Sockets.Socket;


begin

   Context.Initialize (1);
   Socket.Initialize (Context, Sockets.SUB);
   Socket.Establish_message_filter ("");
   Socket.Bind ("tcp://lo:5555");
   Ada.Text_IO.Put_Line ("Connected");
   Read_Loop : loop
      declare
         Buffer : constant String := Socket.recv;
      begin
         Ada.Text_IO.Put_Line (Buffer);
         exit Read_Loop when Buffer = END_MESSAGE;
      end;
   end loop Read_Loop;
end ZMQ.Examples.Display;
