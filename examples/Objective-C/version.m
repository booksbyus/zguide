/* Reports the 0MQ version. */
#import "ZMQObjC.h"

int
main(void)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	int major = 0;
	int minor = 0;
	int patch = 0;
	[ZMQContext getZMQVersionMajor:&major minor:&minor patch:&patch];
	NSLog(@"Current 0MQ version is %d.%d.%d.", major, minor, patch);
	[pool drain];
	return EXIT_SUCCESS;
}
