//  Author:     Mark Kharitonov
//  Email:      Mark.Kharitonov@shunra.co.il

using System;
using System.Text;
using System.Threading;
using ZMQ;

namespace Worker
{
  class Program
  {
    static void Main(string[] args)
    {
      using (var context = new Context(1))
      using (var receiver = context.Socket(SocketType.PULL))
      {
        receiver.Connect("tcp://localhost:5557");

        using (var sender = context.Socket(SocketType.PUSH))
        {
          sender.Connect("tcp://localhost:5558");

          using (var controller = context.Socket(SocketType.SUB))
          {
            controller.Connect("tcp://localhost:5559");
            controller.Subscribe(string.Empty, Encoding.Unicode);

            bool run = true;
            PollItem[] items = new PollItem[2];
            items[0] = receiver.CreatePollItem(IOMultiPlex.POLLIN);
            items[0].PollInHandler += (socket, revents) => ReceiverPollInHandler(socket, sender);
            items[1] = controller.CreatePollItem(IOMultiPlex.POLLIN);
            items[1].PollInHandler += delegate { run = false; };

            //  Process tasks as long as the controller does not signal the end.
            while (run)
            {
              context.Poll(items);
            }
          }
        }
      }
    }

    private static void ReceiverPollInHandler(Socket receiver, Socket sender)
    {
      var ms = receiver.Recv(Encoding.UTF8);
      var timeout = TimeSpan.FromTicks(int.Parse(ms) * 10000L);
      Console.WriteLine(ms);
      Thread.Sleep(timeout);
      sender.Send(string.Empty, Encoding.UTF8);
    }
  }
}