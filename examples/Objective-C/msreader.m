/* msreader.m: Reads from multiple sockets the hard way.
 * *** DON'T DO THIS - see mspoller.m for a better example. *** */
#import "ZMQObjC.h"

static NSString *const kTaskVentEndpoint = @"tcp://localhost:5557";
static NSString *const kWeatherServerEndpoint = @"tcp://localhost:5556";
#define MSEC_PER_NSEC (1000000)

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

	/* Process messages from both sockets, prioritizing the task vent. */
	/* Could fair queue by checking each socket for activity in turn, rather
	 * than continuing to service the current socket as long as it is busy. */
	struct timespec msec = {0, MSEC_PER_NSEC};
	for (;;) {
		/* Worst case: a task is always pending and we never get to weather,
		 * or vice versa. In such a case, memory use would rise without
		 * limit if we did not ensure the objects autoreleased by a single loop
		 * will be autoreleased whether we leave or continue in the loop. */
		NSAutoreleasePool *p;

		/* Process any waiting tasks. */
		for (p = [[NSAutoreleasePool alloc] init];
				nil != [receiver receiveDataWithFlags:ZMQ_NOBLOCK];
				[p drain], p = [[NSAutoreleasePool alloc] init]);
		[p drain];

		/* No waiting tasks - process any waiting weather updates. */
		for (p = [[NSAutoreleasePool alloc] init];
				nil != [subscriber receiveDataWithFlags:ZMQ_NOBLOCK];
				[p drain], p = [[NSAutoreleasePool alloc] init]);
		[p drain];

		/* Nothing doing - sleep for a millisecond. */
		(void)nanosleep(&msec, NULL);
	}

	/* NOT REACHED */
	[ctx closeSockets];
	[pool drain];  /* This finally releases the autoreleased context. */
	return EXIT_SUCCESS;
}
