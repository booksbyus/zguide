/* tasksink.m: PULLs workers' results from tcp://localhost:5558/. */
/* You can wire up the vent, workers, and sink like so:
 *    $ ./tasksink &
 *    $ ./taskwork &  # Repeat this as many times as you want workers.
 *    $ ./taskvent &
 */
#import <Foundation/Foundation.h>
#import "ZMQObjC.h"

#import <sys/time.h>
#define NSEC_PER_MSEC (1000000)
#define MSEC_PER_SEC  (1000)

int
main(void)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

	/* Prepare context and socket. */
	ZMQContext *ctx = [[[ZMQContext alloc] initWithIOThreads:1U] autorelease];
	ZMQSocket *pull = [ctx socketWithType:ZMQ_PULL];
	[pull bindToEndpoint:@"tcp://*:5558"];

	/* New control socket - send any message to kill workers. */
	ZMQSocket *control = [ctx socketWithType:ZMQ_PUB];
	[control bindToEndpoint:@"tcp://*:5559"];

	/* Wait for batch start. */
	/* Cast result to void because we don't actually care about the value.
	 * The return value has been autoreleased, so no memory is leaked. */
	(void)[pull receiveDataWithFlags:0];

	/* Start clock. */
	struct timeval tstart, tdiff, tend;
	(void)gettimeofday(&tstart, NULL);

	/* Process |kTaskCount| confirmations. */
	static const int kTaskCount = 100;
	for (int task = 0; task < kTaskCount; ++task) {
		NSAutoreleasePool *p = [[NSAutoreleasePool alloc] init];

		(void)[pull receiveDataWithFlags:0];

		BOOL isMultipleOfTen = (0 == (task % 10));
		if (isMultipleOfTen) {
			fputs(":", stdout);
		} else {
			fputs(".", stdout);
		}
		fflush(stdout);

		[p drain];
	}
	fputc('\n', stdout);

	/* Stop clock. */
	(void)gettimeofday(&tend, NULL);

	/* Calculate the difference. */
	tdiff.tv_sec = tend.tv_sec - tstart.tv_sec;
	tdiff.tv_usec = tend.tv_usec - tstart.tv_usec;
	if (tdiff.tv_usec < 0) {
		tdiff.tv_sec -= 1;
		tdiff.tv_usec += NSEC_PER_SEC;
	}

	/* Convert it to milliseconds. */
	unsigned long totalMsec = tdiff.tv_sec * MSEC_PER_SEC
			+ tdiff.tv_usec / NSEC_PER_MSEC;
	NSLog(@"Total elapsed time: %lu ms", totalMsec);

	/* Kill workers. Any message will do, including an empty one. */
	[control sendData:nil withFlags:0];

	/* Give 0MQ time to deliver the kill message. */
	sleep(1);

	[ctx closeSockets];
	[pool drain];
	return EXIT_SUCCESS;
}
