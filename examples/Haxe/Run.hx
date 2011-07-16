package ;

import neko.io.File;
import neko.io.FileInput;
import neko.io.Process;
import neko.Lib;
import neko.Sys;


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
			Lib.println("11. Interrupt");
			Lib.println("");
			Lib.println("12. MTServer");
			Lib.println("");
			Lib.println("13. TaskWork2");
            Lib.println("14. TaskSink2");
			Lib.println("");
			Lib.println("15. WUProxy");
			Lib.println("");
			Lib.println("16. RrClient");
			Lib.println("17. RrBroker");
			Lib.println("18. RrServer");
			Lib.println("");
			Lib.println("19. MsgQueue");
			Lib.println("");			
			Lib.println("20. MTRelay");
            Lib.println("");
            Lib.println("21. SyncPub");
            Lib.println("22. SyncSub");
            Lib.println("");
            Lib.println("23. PSEnvPub");
            Lib.println("24. PSEnvSub");
            Lib.println("");
            Lib.println("25. DuraPub");
            Lib.println("26. DuraSub");
            Lib.println("27. DuraPub2");
			Lib.println("");
			Lib.println("28. Identity");
			Lib.println("");
			Lib.println("29. RTDealer");
			Lib.println("");
			Lib.println("30. RTMama");
			Lib.println("");
			Lib.println("31. RTPapa");
			Lib.println("");
			Lib.println("32. LRUQueue");
			Lib.println("");
			
			
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
				MsgQueue.main();
            case 20:
                MTRelay.main();
            case 21:
                SyncPub.main();
            case 22:
                SyncSub.main();
            case 23:
                PSEnvPub.main();
            case 24:
                PSEnvSub.main();
            case 25:
                DuraPub.main();
            case 26:
                DuraSub.main();
            case 27:
                DuraPub2.main();
			case 28:
				Identity.main();
			case 29:
				RTDealer.main();
			case 30:
				RTMama.main();
			case 31:
				RTPapa.main();
			case 32:
				LRUQueue.main();
			default:
			Lib.println ("Unknown program number ... exiting");
		}
	}
	
}
