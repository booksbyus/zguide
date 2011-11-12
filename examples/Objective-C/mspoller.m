/* msreader.m: Reads from multiple sockets the right way. */
#import "ZMQObjC.h"

static NSString *const kTaskVentEndpoint = @"tcp://localhost:5557";
static NSString *const kWeatherServerEndpoint = @"tcp://localhost:5556";

int
main(void)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	ZMQContext *ctx = [[[ZMQContext alloc] initWithIOThreads:1U] autorelease];

	/* Connect to task ventilator. */
	ZMQSocket *receiver = [ctx socketWithType:ZMQ_PULL];
	[receiver connectToEndpoint:kTaskVentEndpoint];

	/* Connect to weather server. */
	ZMQSocket *subscriber = [ctx socketWithType:ZMQ_SUB];
	[subscriber connectToEndpoint:kWeatherServerEndpoint];
	NSData *subData = [@"10001" dataUsingEncoding:NSUTF8StringEncoding];
	[subscriber setData:subData forOption:ZMQ_SUBSCRIBE];

	/* Initialize poll set. */
	zmq_pollitem_t items[2];
	[receiver getPollItem:&items[0] forEvents:ZMQ_POLLIN];
	[subscriber getPollItem:&items[1] forEvents:ZMQ_POLLIN];

	/* Process messages from both sockets. */
	for (;;) {
		NSAutoreleasePool *p = [[NSAutoreleasePool alloc] init];
		[ZMQContext pollWithItems:items count:2
				timeoutAfterUsec:ZMQPollTimeoutNever];
		[p drain];
	}

	/* NOT REACHED */
	[ctx closeSockets];
	[pool drain];  /* This finally releases the autoreleased context. */
	return EXIT_SUCCESS;
}
