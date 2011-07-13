package ;

import neko.io.File;
import neko.io.FileInput;
import neko.io.Process;
import neko.Lib;
import neko.Sys;

import org.zeromq.guide.HelloWorldClient;
import org.zeromq.guide.HelloWorldServer;
import org.zeromq.guide.WUClient;
import org.zeromq.guide.WUServer;
import org.zeromq.guide.TaskVent;
import org.zeromq.guide.TaskWork;
import org.zeromq.guide.TaskSink;
import org.zeromq.guide.Interrupt;

/**
 * Main class that allows any of the implemented Haxe guide programs to run
 */
class Run 
{

	public static function main() 
	{
		var selection:Int;
		
		Lib.println("** HaXe ZeroMQ Guide program launcher **");
		
		if (Sys.args().length > 0) {
			selection = Std.parseInt(Sys.args()[0]);
		} else {
			Lib.println("");
			Lib.println("Programs:");
			
			Lib.println("1. HelloWorldClient");
			Lib.println("2. HelloWorldServer");
			Lib.println("");
			Lib.println("3. WUClient");
			Lib.println("4. WUServer");
			Lib.println("");
			Lib.println("5. TaskVent");
			Lib.println("6. TaskWork");
			Lib.println("7. TaskSink");
			Lib.println("");
			Lib.println("11. Interrupt (** Doesn't work on Windows!)");
			Lib.println("");
			Lib.println("12. MTServer (use with 1. HelloWorldClient)");
			Lib.println("");
			Lib.println("13. TaskWork2 (use with 5. TaskVent and 14. TaskSink2)");
            Lib.println("14. TaskSink2 (use with 5. TaskVent and 13. TaskWork2)");
			Lib.println("");
			Lib.println("15. WUProxy (use with 4. WUServer)");
			Lib.println("");
			Lib.println("16. RrClient");
			Lib.println("17. RrBroker");
			Lib.println("18. RrServer");
			Lib.println("");
			Lib.println("19. MTRelay");
            Lib.println("");
            Lib.println("20. SyncPub");
            Lib.println("21. SyncSub");
            Lib.println("");
            Lib.println("22. PSEnvPub");
            Lib.println("23. PSEnvSub");
            Lib.println("");
            Lib.println("24. DuraPub");
            Lib.println("25. DuraSub");
            Lib.println("26. DuraPub2");
			
			do {
				Lib.print("Type number followed by Enter key, or q to quit: ");
				var f:FileInput = File.stdin();
				var str:String = f.readLine();
		
				if (str.toLowerCase() == "q") {
					return;
				}
				
				selection = Std.parseInt(str);
			} while (selection == null);
		}
		
		switch (selection) {
			case 1:
				HelloWorldClient.main();
			case 2:
				HelloWorldServer.main();
			case 3:
				WUClient.main();
			case 4:
				WUServer.main();
			case 5:
				TaskVent.main();
			case 6:
				TaskWork.main();
			case 7:
				TaskSink.main();
			case 11:
				Interrupt.main();
			case 12:
				MTServer.main();
            case 13:
                TaskWork2.main();
            case 14:
                TaskSink2.main();
            case 15:
                WUProxy.main();
            case 16:
                RrClient.main();
            case 17:
                RrBroker.main();
            case 18:
                RrServer.main();
            case 19:
                MTRelay.main();
            case 20:
                SyncPub.main();
            case 21:
                SyncSub.main();
            case 22:
                PSEnvPub.main();
            case 23:
                PSEnvSub.main();
            case 24:
                DuraPub.main();
            case 25:
                DuraSub.main();
            case 26:
                DuraPub2.main();
			default:
			Lib.println ("Unknown program number ... exiting");
		}
	}
	
}
