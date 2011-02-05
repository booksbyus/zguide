/* Task ventilator - sends task batch to workers via PUSH socket. */
#import <Foundation/Foundation.h>
#import "ZMQObjC.h"
#import "ZMQHelper.h"

int
main(void)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	ZMQContext *ctx = [[[ZMQContext alloc] initWithIOThreads:1U] autorelease];
	ZMQSocket *sender = [ctx socketWithType:ZMQ_PUSH];
	[sender bindToEndpoint:@"tcp://*:5557"];

	NSLog(@"Press Enter when the workers are ready: ");
	(void)getchar();
	NSLog(@"Sending tasks to workers...");

	/* Signal batch start with message of "0". */
	NSData *signalData = [@"0" dataUsingEncoding:NSUTF8StringEncoding];
	[sender sendData:signalData withFlags:0];

	/* Initialize random number generator. */
	(void)srandom((unsigned)time(NULL));

	/* Send kTaskCount tasks. */
	unsigned long totalMsec = 0UL;
	static const int kTaskCount = 100;
	for (int task = 0; task < kTaskCount; ++task) {
		/* Random workload from 1 to 100 msec. */
		int workload = within(100) + 1;
		totalMsec += workload;
		NSString *text = [NSString stringWithFormat:@"%d", workload];
		NSData *textData = [text dataUsingEncoding:NSUTF8StringEncoding];
		[sender sendData:textData withFlags:0];
	}
	NSLog(@"Total expected cost: %lu ms", totalMsec);

	/* Let IOThreads finish sending. */
	sleep(1);

	[sender close];
	[pool drain];
	return EXIT_SUCCESS;
}
