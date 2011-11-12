//
//  Hello World server
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
//
#import <Foundation/Foundation.h>
#import "ZMQObjc.h"

int
main(void)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	ZMQContext *ctx = [[[ZMQContext alloc] initWithIOThreads:1U] autorelease];

	/* Get a socket to talk to clients. */
	static NSString *const kEndpoint = @"tcp://*:5555";
	ZMQSocket *responder = [ctx socketWithType:ZMQ_REP];
	BOOL didBind = [responder bindToEndpoint:kEndpoint];
	if (!didBind) {
		NSLog(@"*** Failed to bind to endpoint [%@].", kEndpoint);
		return EXIT_FAILURE;
	}

	for (;;) {
		/* Create a local pool so that autoreleased objects can be disposed of
		 * at the end of each go through the loop.
		 * Otherwise, memory usage would continue to rise
		 * until the end of the process.
		 */
		NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];

		/* Block waiting for next request from client. */
		NSData *request = [responder receiveDataWithFlags:0];
		NSString *text = [[[NSString alloc]
				initWithData:request encoding:NSUTF8StringEncoding] autorelease];
		NSLog(@"Received request: %@", text);

		/* "Work" for a bit. */
		sleep(1);

		/* Send reply back to client. */
		static NSString *const kWorld = @"World";
		const char *replyCString = [kWorld UTF8String];
		const NSUInteger replyLength = [kWorld
				lengthOfBytesUsingEncoding:NSUTF8StringEncoding];
		NSData *reply = [NSData dataWithBytes:replyCString length:replyLength];
		[responder sendData:reply withFlags:0];

		[localPool drain];
	}

	/* Close the socket to avoid blocking in -[ZMQContext terminate]. */
	[responder close];
	/* Dispose of the context and socket. */
	[pool drain];
	return EXIT_SUCCESS;
}
