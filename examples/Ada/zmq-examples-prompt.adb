with ZMQ.Sockets;
with ZMQ.Contexts;
with Ada.Text_IO;
with GNAT.Sockets;
procedure ZMQ.examples.prompt is

   ctx : aliased Contexts.Context;
   s   : Sockets.Socket;

begin
   ctx.Initialize (1);
   s.Initialize (ctx, Sockets.PUB);
   s.Connect ("tcp://localhost:5555");

   Read_Loop : loop
      Ada.Text_IO.Put (">");
      declare
         textbuf : constant String :=  Ada.Text_IO.Get_Line;
      begin
         exit Read_Loop when textbuf'Length = 0;
         s.Send ("hej" & ASCII.NUL & GNAT.Sockets.Host_Name & ":" & textbuf);
         delay 0.02;
      end;
   end loop Read_Loop;
   s.Send (END_MESSAGE);
end ZMQ.Examples.prompt;
