//
//  Hello World client
//  Connects REQ socket to tcp://localhost:5555
//  Sends "Hello" to server, expects "World" back
//
#import "ZMQObjC.h"

int
main (void)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	ZMQContext *ctx = [[[ZMQContext alloc] initWithIOThreads:1U] autorelease];
	
	/* Get a socket to talk to clients. */
	NSLog(@"Connecting to hello world server...");
	static NSString *const kEndpoint = @"tcp://localhost:5555";
	ZMQSocket *requester = [ctx socketWithType:ZMQ_REQ];
	BOOL didBind = [requester connectToEndpoint:kEndpoint];
	if (!didBind) {
		NSLog(@"*** Failed to bind to endpoint [%@].", kEndpoint);
		return EXIT_FAILURE;
	}

	static const int kMaxRequest = 10;
	NSData *const request = [@"Hello" dataUsingEncoding:NSUTF8StringEncoding];
	for (int request_nbr = 0; request_nbr < kMaxRequest; ++request_nbr) {
		NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];

		NSLog(@"Sending request %d.", request_nbr);
		[requester sendData:request withFlags:0];
		NSData *reply = [requester receiveDataWithFlags:0];
		NSString *text = [[[NSString alloc]
				initWithData:reply encoding:NSUTF8StringEncoding] autorelease];
		NSLog(@"Received reply %d: %@", request_nbr, text);

		[localPool drain];
	}

	[requester close];
	[pool drain];
	return EXIT_SUCCESS;
}
