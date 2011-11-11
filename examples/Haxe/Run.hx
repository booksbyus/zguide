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
			Lib.println("30. RTMama");
			Lib.println("31. RTPapa");
			Lib.println("32. LRUQueue");
			Lib.println("33. LRUQueue2");
			Lib.println("34. LRUQueue3");
			Lib.println("35. ASyncSrv");
			Lib.println("");
			Lib.println("36. Peering1");
			Lib.println("37. Peering2");
			Lib.println("38. Peering3");
			Lib.println("");
			Lib.println("39. LPClient");
			Lib.println("40. LPServer");
			Lib.println("41. SPQueue");
			Lib.println("42. SPWorker");
			Lib.println("43. PPQueue");
			Lib.println("44. PPWorker");
			Lib.println("");
			Lib.println("45. MDClient");
			Lib.println("46. MDWorker");
			Lib.println("47. MDBroker");
			Lib.println("48. Tripping");
			Lib.println("49. MDClient2");
			Lib.println("50. MMIEcho");
			Lib.println("51. TIClient");
			Lib.println("52. Titanic");
			Lib.println("");
			Lib.println("53. BStarSrv");
			Lib.println("54. BStarCli");
			Lib.println("55. BStarSrv2");
			
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
			case 33:
				LRUQueue2.main();
			case 34:
				LRUQueue3.main();
			case 35:
				ASyncSrv.main();
			case 36:
				Peering1.main();
			case 37:
				Peering2.main();
			case 38:
				Peering3.main();
			case 39:
				LPClient.main();
			case 40:
				LPServer.main();
			case 41:
				SPQueue.main();
			case 42:
				SPWorker.main();
			case 43:
				PPQueue.main();
			case 44:
				PPWorker.main();
			case 45:
				MDClient.main();
			case 46:
				MDWorker.main();
			case 47:
				MDBroker.main();
			case 48:
				Tripping.main();
			case 49:
				MDClient2.main();
			case 50:
				MMIEcho.main();
			case 51:
				TIClient.main();
			case 52:
				Titanic.main();
			case 53:
				BStarSrv.main();
			case 54:
				BStarCli.main();
			case 55:
				BStarSrv2.main();
			default:
				Lib.println ("Unknown program number ... exiting");
		}
	}
	
}
