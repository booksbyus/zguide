//
//  Weather update server
//  Binds PUB socket to tcp://*:5556
//  Publishes random weather updates
//
#import "ZMQObjC.h"
#import "ZMQHelper.h"

int
main(void)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

	//  Prepare our context and publisher
	ZMQContext *ctx = [[[ZMQContext alloc] initWithIOThreads:1U] autorelease];
	ZMQSocket *publisher = [ctx socketWithType:ZMQ_PUB];
	[publisher bindToEndpoint:@"tcp://*:5556"];
	[publisher bindToEndpoint:@"ipc://weather.ipc"];

	//  Initialize random number generator
	srandom ((unsigned) time (NULL));
	for (;;) {
		//  Get values that will fool the boss
		int zipcode, temperature, relhumidity;
		zipcode     = within (100000);
		temperature = within (215) - 80;
		relhumidity = within (50) + 10;

		// Send message to all subscribers
		NSString *update = [NSString stringWithFormat:@"%05d %d %d",
				zipcode, temperature, relhumidity];
		NSData *data = [update dataUsingEncoding:NSUTF8StringEncoding];
		[publisher sendData:data withFlags:0];
	}
	[publisher close];
	[pool drain];
	return EXIT_SUCCESS;
}
