//  Create a new Binary Star instance, using local (bind) and
//  remote (connect) endpoints to set-up the server peering.
bstar bs = new bstar(bool primary, String local, String remote);

//  Destroy a Binary Star instance
bstar.destory();

//  Return underlying zloop reactor, for timer and reader
//  registration and cancelation.
ZLoop bstar.zloop();

//  Register voting reader
int bstar.voter(String endpoint, int type,
                 ZLoop.IZLoopHandler handler, Object arg);

//  Register main state change handlers
void bstar.newActive (ZLoop.IZLoopHandler handler, Object arg);
void bstar.newPassive (ZLoop.IZLoopHandler handler, Object arg);

//  Start the reactor, ends if a callback function returns -1, or the
//  process received Interrupt .
int bstar.start();
