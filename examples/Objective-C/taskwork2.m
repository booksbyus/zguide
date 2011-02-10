/* taskwork2.m: PULLs workload from tcp://localhost:5557
 * PUSHes results to tcp://localhost:5558
 * SUBs to tcp://localhost:5559 to receive kill signal (*** NEW ***)
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

	ZMQSocket *control = [ctx socketWithType:ZMQ_SUB];
	[control setData:nil forOption:ZMQ_SUBSCRIBE];
	[control connectToEndpoint:@"tcp://localhost:5559"];

	/* Process tasks forever, multiplexing between |pull| and |control|. */
	enum {POLL_PULL, POLL_CONTROL};
	zmq_pollitem_t items[2];
	[pull getPollItem:&items[POLL_PULL] forEvents:ZMQ_POLLIN];
	[control getPollItem:&items[POLL_CONTROL] forEvents:ZMQ_POLLIN];
	size_t itemCount = sizeof(items)/sizeof(*items);

	struct timespec t;
	NSData *emptyData = [NSData data];
	bool shouldExit = false;
	while (!shouldExit) {
		NSAutoreleasePool *p = [[NSAutoreleasePool alloc] init];

		[ZMQContext pollWithItems:items count:itemCount
				timeoutAfterUsec:ZMQPollTimeoutNever];
		if (items[POLL_PULL].revents & ZMQ_POLLIN) {
			NSData *d = [pull receiveDataWithFlags:0];
			NSString *s = [NSString stringWithUTF8String:[d bytes]];
			t.tv_sec = 0;
			t.tv_nsec = [s integerValue] * NSEC_PER_MSEC;
			printf("%d.", [s intValue]);
			fflush(stdout);

			/* Do work, then report finished. */
			(void)nanosleep(&t, NULL);
			[push sendData:emptyData withFlags:0];
		}

		/* Any inbound data on |control| signals us to die. */
		if (items[POLL_CONTROL].revents & ZMQ_POLLIN) {
			/* Do NOT just break here: |p| must be drained first. */
			shouldExit = true;
		}
		[p drain];
	}

	[ctx closeSockets];
	[pool drain];
	return EXIT_SUCCESS;
}
