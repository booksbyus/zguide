//
//  Weather update client
//  Connects SUB socket to tcp://localhost:5556
//  Collects weather updates and finds avg temp in zipcode
//
#import "ZMQObjC.h"

int
main(int argc, const char *argv[])
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	ZMQContext *ctx = [[[ZMQContext alloc] initWithIOThreads:1U] autorelease];

	// Socket to talk to server
	ZMQSocket *subscriber = [ctx socketWithType:ZMQ_SUB];
	if (![subscriber connectToEndpoint:@"tcp://localhost:5556"]) {
		/* ZMQSocket will already have logged the error. */
		return EXIT_FAILURE;
	}

	/* Subscribe to zipcode (defaults to NYC, 10001). */
	const char *kNYCZipCode = "10001";
	const char *filter = (argc > 1)? argv[1] : kNYCZipCode;
	NSData *filterData = [NSData dataWithBytes:filter length:strlen(filter)];
	[subscriber setData:filterData forOption:ZMQ_SUBSCRIBE];

	/* Write to stdout immediately rather than at each newline.
	 * This makes the incremental temperatures appear incrementally.
	 */
	(void)setvbuf(stdout, NULL, _IONBF, BUFSIZ);

	/* Process updates. */
	NSLog(@"Collecting temperatures for zipcode %s from weather server...", filter);
	const int kMaxUpdate = 100;
	long total_temp = 0;
	for (int update_nbr = 0; update_nbr < kMaxUpdate; ++update_nbr) {
		NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

		NSData *msg = [subscriber receiveDataWithFlags:0];
		const char *string = [msg bytes];

		int zipcode = 0, temperature = 0, relhumidity = 0;
		(void)sscanf(string, "%d %d %d", &zipcode, &temperature, &relhumidity);

		printf("%d ", temperature);
		total_temp += temperature;

		[pool drain];
	}
	/* End line of temperatures. */
	putchar('\n');

	NSLog(@"Average temperature for zipcode '%s' was %ld degF.",
			filter, total_temp / kMaxUpdate);

	/* [ZMQContext sockets] makes it easy to close all associated sockets. */
	[[ctx sockets] makeObjectsPerformSelector:@selector(close)];
	[pool drain];
	return EXIT_SUCCESS;
}
