with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.devices;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure ZMQ.examples.Multi_Thread_Server is

   task type server_task (ctx : not null access ZMQ.Contexts.Context;
                          id  : Integer) is
   end server_task;

   task body server_task is
      msg : Ada.Strings.Unbounded.Unbounded_String;
      s   : ZMQ.Sockets.Socket;
   begin
      s.Initialize (ctx.all, Sockets.REP);
      s.Connect ("inproc://workers");
      loop
         msg := s.recv;
         Append (msg, "<Served by thread:" & id'Img & ">");
         s.Send (msg);
      end loop;
   end server_task;

   ctx              : aliased ZMQ.Contexts.Context;

   Number_Of_Servers : constant := 10;
   servers          : array (1 .. Number_Of_Servers) of access server_task;

   workers          : ZMQ.Sockets.Socket;
   clients          : ZMQ.Sockets.Socket;

   dev              : ZMQ.devices.device;

begin
   --  Initialise 0MQ context, requesting a single application thread
   --  and a single I/O thread
   ctx.Initialize (servers'Length + 1);

   --   Create a ZMQ_REP socket to receive requests and send replies
   workers.Initialize (ctx, Sockets.DEALER);
   workers.Bind ("inproc://workers");

   --   Bind to the TCP transport and port 5555 on the 'lo' interface
   clients.Initialize (ctx, Sockets.ROUTER);
   clients.Bind ("tcp://lo:5555");

   for i in servers'Range loop
      servers (i) := new server_task (ctx'Access, i);
   end loop;

   dev.initialize (devices.Queue, clients, workers);

end ZMQ.Examples.Multi_Thread_Server;
