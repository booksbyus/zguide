/* taskwork.m: PULLs workload from tcp://localhost:5557
 * PUSHes results to tcp://localhost:5558
 */
#import <Foundation/Foundation.h>
#import "ZMQObjC.h"
#define NSEC_PER_MSEC (1000000)

int
main(void)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	ZMQContext *ctx = [[[ZMQContext alloc] initWithIOThreads:1U] autorelease];

	/* (jws/2011-02-05)!!!: Do NOT terminate the endpoint with a final slash.
	 * If you connect to @"tcp://localhost:5557/", you will get
	 *    Assertion failed: rc == 0 (zmq_connecter.cpp:46)
	 * instead of a connected socket. Binding works fine, though. */
	ZMQSocket *pull = [ctx socketWithType:ZMQ_PULL];
	[pull connectToEndpoint:@"tcp://localhost:5557"];

	ZMQSocket *push = [ctx socketWithType:ZMQ_PUSH];
	[push connectToEndpoint:@"tcp://localhost:5558"];

	/* Process tasks forever. */
	struct timespec t;
	NSData *emptyData = [NSData data];
	for (;;) {
		NSAutoreleasePool *p = [[NSAutoreleasePool alloc] init];

		NSData *d = [pull receiveDataWithFlags:0];
		NSString *s = [NSString stringWithUTF8String:[d bytes]];
		t.tv_sec = 0;
		t.tv_nsec = [s integerValue] * NSEC_PER_MSEC;
		printf("%d.", [s intValue]);
		fflush(stdout);

		/* Do work, then report finished. */
		(void)nanosleep(&t, NULL);
		[push sendData:emptyData withFlags:0];

		[p drain];
	}

	[ctx closeSockets];
	[pool drain];
	return EXIT_SUCCESS;
}
