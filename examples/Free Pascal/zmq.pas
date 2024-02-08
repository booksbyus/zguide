{
  fpc-zmq
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU Lesser General Public License (LGPL v3.0).

  You should have received a copy of the GNU Lesser General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit zmq; // https://github.com/zeromq/libzmq/commit/f1c72dc8e5a92c32013a07d8aee68deee70adf8a

{$mode objfpc}{$H+}

{$IFDEF LINUX}
  {$LINKLIB pthread}
  {$IFDEF ZMQ_STATIC_LINK}
    {$LINKLIB m}
    {$LINKLIB stdc++}
    {$LINKLIB gcc_s}
    {$LINKLIB zmq.a} // /usr/local/lib/libzmq.a
  {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}
  {$IFDEF ZMQ_STATIC_LINK}
    // libzmq.lib
    // implement me
  {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}
  {$IFDEF ZMQ_STATIC_LINK}
    // libzmq.a
    // implement me
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  {$IFDEF ZMQ_STATIC_LINK}
    // libzmq.a
    // implement me
  {$ENDIF}
{$ENDIF}

interface

{$IFNDEF ZMQ_STATIC_LINK}
const
  {$ifdef LINUX}
  libzmq = 'zmq';
  {$ENDIF}

  {$IFDEF WINDOWS}
  libzmq = 'libzmq.dll';
  {$ENDIF}

  {$IFDEF DARWIN}
  libzmq = 'libzmq.dylib';
  {$ENDIF}
  
  {$ifdef ANDROID}
  libzmq = 'libzmq.so';
  {$ENDIF}
  
{$ENDIF}

{
  /*  Define integer types needed for event interface                          */
}
const
  ZMQ_DEFINED_STDINT = 1;

{
  /******************************************************************************/
  /*  0MQ errors.                                                               */
  /******************************************************************************/

  /*  A number random enough not to collide with different errno ranges on      */
  /*  different OSes. The assumption is that error_t is at least 32-bit type.   */
}

  ZMQ_HAUSNUMERO = 156384712;

{
  /*  On Windows platform some of the standard POSIX errnos are not defined.    */
}

  ENOTSUP = ZMQ_HAUSNUMERO + 1;
  EPROTONOSUPPORT = ZMQ_HAUSNUMERO + 2;
  ENOBUFS = ZMQ_HAUSNUMERO + 3;
  ENETDOWN = ZMQ_HAUSNUMERO + 4;
  EADDRINUSE = ZMQ_HAUSNUMERO + 5;
  EADDRNOTAVAIL = ZMQ_HAUSNUMERO + 6;
  ECONNREFUSED = ZMQ_HAUSNUMERO + 7;
  EINPROGRESS = ZMQ_HAUSNUMERO + 8;
  ENOTSOCK = ZMQ_HAUSNUMERO + 9;
  EMSGSIZE = ZMQ_HAUSNUMERO + 10;
  EAFNOSUPPORT = ZMQ_HAUSNUMERO + 11;
  ENETUNREACH = ZMQ_HAUSNUMERO + 12;
  ECONNABORTED = ZMQ_HAUSNUMERO + 13;
  ECONNRESET = ZMQ_HAUSNUMERO + 14;
  ENOTCONN = ZMQ_HAUSNUMERO + 15;
  ETIMEDOUT = ZMQ_HAUSNUMERO + 16;
  EHOSTUNREACH = ZMQ_HAUSNUMERO + 17;
  ENETRESET = ZMQ_HAUSNUMERO + 18;

{
  /*  Native 0MQ error codes.                                                   */
}

  EFSM = ZMQ_HAUSNUMERO + 51;
  ENOCOMPATPROTO = ZMQ_HAUSNUMERO + 52;
  ETERM = ZMQ_HAUSNUMERO + 53;
  EMTHREAD = ZMQ_HAUSNUMERO + 54;

{
  /*  This function retrieves the errno as it is known to 0MQ library. The goal */
  /*  of this function is to make the code 100% portable, including where 0MQ   */
  /*  compiled with certain CRT library (on Windows) is linked to an            */
  /*  application that uses different CRT library.                              */
}
function zmq_errno: integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /*  Resolves system errors and 0MQ errors to human-readable string.           */
}
function zmq_strerror(errnum: integer):PChar; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /* Run-time API version detection                                             */
}
procedure zmq_version(out major: integer; out minor: integer; out patch: integer); cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /******************************************************************************/
  /*  0MQ infrastructure (a.k.a. context) initialisation & termination.         */
  /******************************************************************************/

  /*  Context options                                                           */
}
const

ZMQ_IO_THREADS  = 1;
ZMQ_MAX_SOCKETS = 2;
ZMQ_SOCKET_LIMIT = 3;
ZMQ_THREAD_PRIORITY = 3;
ZMQ_THREAD_SCHED_POLICY = 4;
ZMQ_MAX_MSGSZ = 5;

{
  /*  Default for new contexts                                                  */
}

ZMQ_IO_THREADS_DFLT  = 1;
ZMQ_MAX_SOCKETS_DFLT = 1023;
ZMQ_THREAD_PRIORITY_DFLT = -1;
ZMQ_THREAD_SCHED_POLICY_DFLT = -1;

function zmq_ctx_new : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_ctx_term(context: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_ctx_shutdown(context: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_ctx_set(context: Pointer; option: integer; optval: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_ctx_get(context: Pointer; option: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /*  Old (legacy) API                                                          */
}

function zmq_init(io_threads: integer) : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_term (context: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_ctx_destroy (context: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

//
//  /******************************************************************************/
//  /*  0MQ message definition.                                                   */
//  /******************************************************************************/
//
//  /* Some architectures, like sparc64 and some variants of aarch64, enforce pointer
//   * alignment and raise sigbus on violations. Make sure applications allocate
//   * zmq_msg_t on addresses aligned on a pointer-size boundary to avoid this issue.
//   */
//

// http://forum.lazarus.freepascal.org/index.php/topic,37739.0.html

{$IFDEF ANDROID}
  {$IFDEF ARM64}{$IFDEF CPU64}{$ALIGN 8}{$ENDIF}{$ENDIF}
  {$IFDEF ARMV7VE}{$IFDEF CPU32}{$ALIGN 4}{$ENDIF}{$ENDIF}
{$ENDIF}
type zmq_msg_t = packed record
  _ : array [0..64-1] of Byte;
end;
{$IFDEF ANDROID}
  {$ALIGN OFF}
{$ENDIF}

// type Pzmq_msg_t = ^zmq_msg_t;

type zmq_free_fn = procedure (data: Pointer; hint: pointer);

function zmq_msg_init(out msg: zmq_msg_t) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_init_size(out msg: zmq_msg_t; size: LongWord) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_init_data(out msg: zmq_msg_t; data: Pointer;
  size: LongWord; ffn : zmq_free_fn; hint : Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_send(var msg: zmq_msg_t;  s: Pointer; flags: integer): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_recv(var msg: zmq_msg_t; s: Pointer; flags: integer): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_close(var msg: zmq_msg_t): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_move(var dest: zmq_msg_t; var src: zmq_msg_t): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_copy(var dest: zmq_msg_t; var src: zmq_msg_t): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_data(var msg: zmq_msg_t ) : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_size(var msg: zmq_msg_t) : LongWord; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_more(var msg: zmq_msg_t): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_get(var msg: zmq_msg_t; aproperty: integer): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_set(var msg: zmq_msg_t; aproperty: integer; optval: integer): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
function zmq_msg_gets(var msg: zmq_msg_t; const aproperty: PChar): PChar; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /******************************************************************************/
  /*  0MQ socket definition.                                                    */
  /******************************************************************************/

  /*  Socket types.                                                             */
}
const
  ZMQ_PAIR = 0;
  ZMQ_PUB = 1;
  ZMQ_SUB = 2;
  ZMQ_REQ = 3;
  ZMQ_REP = 4;
  ZMQ_DEALER = 5;
  ZMQ_ROUTER = 6;
  ZMQ_PULL = 7;
  ZMQ_PUSH = 8;
  ZMQ_XPUB = 9;
  ZMQ_XSUB = 10;
  ZMQ_STREAM = 11;

{
  /*  Deprecated aliases                                                        */
}

  ZMQ_XREQ = ZMQ_DEALER deprecated;
  ZMQ_XREP = ZMQ_ROUTER deprecated;

{
  /*  Socket options.                                                           */
}

  ZMQ_AFFINITY = 4;
  ZMQ_IDENTITY = 5;
  ZMQ_SUBSCRIBE = 6;
  ZMQ_UNSUBSCRIBE = 7;
  ZMQ_RATE = 8;
  ZMQ_RECOVERY_IVL = 9;
  ZMQ_SNDBUF = 11;
  ZMQ_RCVBUF = 12;
  ZMQ_RCVMORE = 13;
  ZMQ_FD = 14;
  ZMQ_EVENTS = 15;
  ZMQ_TYPE = 16;
  ZMQ_LINGER = 17;
  ZMQ_RECONNECT_IVL = 18;
  ZMQ_BACKLOG = 19;
  ZMQ_RECONNECT_IVL_MAX = 21;
  ZMQ_MAXMSGSIZE = 22;
  ZMQ_SNDHWM = 23;
  ZMQ_RCVHWM = 24;
  ZMQ_MULTICAST_HOPS = 25;
  ZMQ_RCVTIMEO = 27;
  ZMQ_SNDTIMEO = 28;
  ZMQ_LAST_ENDPOINT = 32;
  ZMQ_ROUTER_MANDATORY = 33;
  ZMQ_TCP_KEEPALIVE = 34;
  ZMQ_TCP_KEEPALIVE_CNT = 35;
  ZMQ_TCP_KEEPALIVE_IDLE = 36;
  ZMQ_TCP_KEEPALIVE_INTVL = 37;
  ZMQ_IMMEDIATE = 39;
  ZMQ_XPUB_VERBOSE = 40;
  ZMQ_ROUTER_RAW = 41;
  ZMQ_IPV6 = 42;
  ZMQ_MECHANISM = 43;
  ZMQ_PLAIN_SERVER = 44;
  ZMQ_PLAIN_USERNAME = 45;
  ZMQ_PLAIN_PASSWORD = 46;
  ZMQ_CURVE_SERVER = 47;
  ZMQ_CURVE_PUBLICKEY = 48;
  ZMQ_CURVE_SECRETKEY = 49;
  ZMQ_CURVE_SERVERKEY = 50;
  ZMQ_PROBE_ROUTER = 51;
  ZMQ_REQ_CORRELATE = 52;
  ZMQ_REQ_RELAXED = 53;
  ZMQ_CONFLATE = 54;
  ZMQ_ZAP_DOMAIN = 55;
  ZMQ_ROUTER_HANDOVER = 56;
  ZMQ_TOS = 57;
  ZMQ_CONNECT_RID = 61;
  ZMQ_GSSAPI_SERVER = 62;
  ZMQ_GSSAPI_PRINCIPAL = 63;
  ZMQ_GSSAPI_SERVICE_PRINCIPAL = 64;
  ZMQ_GSSAPI_PLAINTEXT = 65;
  ZMQ_HANDSHAKE_IVL = 66;
  ZMQ_SOCKS_PROXY = 68;
  ZMQ_XPUB_NODROP = 69;
  ZMQ_BLOCKY = 70;
  ZMQ_XPUB_MANUAL = 71;
  ZMQ_XPUB_WELCOME_MSG = 72;
  ZMQ_STREAM_NOTIFY = 73;
  ZMQ_INVERT_MATCHING = 74;
  ZMQ_HEARTBEAT_IVL = 75;
  ZMQ_HEARTBEAT_TTL = 76;
  ZMQ_HEARTBEAT_TIMEOUT = 77;
  ZMQ_XPUB_VERBOSER = 78;
  ZMQ_CONNECT_TIMEOUT = 79;
  ZMQ_TCP_MAXRT = 80;
  ZMQ_THREAD_SAFE = 81;
  ZMQ_MULTICAST_MAXTPDU = 84;
  ZMQ_VMCI_BUFFER_SIZE = 85;
  ZMQ_VMCI_BUFFER_MIN_SIZE = 86;
  ZMQ_VMCI_BUFFER_MAX_SIZE = 87;
  ZMQ_VMCI_CONNECT_TIMEOUT = 88;
  ZMQ_USE_FD = 89;

{
  /*  Message options                                                           */
}

  ZMQ_MORE = 1;
  ZMQ_SHARED = 3;

{
  /*  Send/recv options.                                                        */
}

  ZMQ_DONTWAIT = 1;
  ZMQ_SNDMORE = 2;

{
  /*  Security mechanisms                                                       */
}
  ZMQ_NULL = 0;
  ZMQ_PLAIN = 1;
  ZMQ_CURVE = 2;
  ZMQ_GSSAPI = 3;

{
  /*  RADIO-DISH protocol                                                       */
}
  ZMQ_GROUP_MAX_LENGTH = 15;

{
  /*  Deprecated options and aliases                                            */
}

  ZMQ_TCP_ACCEPT_FILTER       = 38 deprecated;
  ZMQ_IPC_FILTER_PID          = 58 deprecated;
  ZMQ_IPC_FILTER_UID          = 59 deprecated;
  ZMQ_IPC_FILTER_GID          = 60 deprecated;
  ZMQ_IPV4ONLY                = 31 deprecated;
  ZMQ_DELAY_ATTACH_ON_CONNECT = ZMQ_IMMEDIATE deprecated;
  ZMQ_NOBLOCK                 = ZMQ_DONTWAIT deprecated;
  ZMQ_FAIL_UNROUTABLE         = ZMQ_ROUTER_MANDATORY deprecated;
  ZMQ_ROUTER_BEHAVIOR         = ZMQ_ROUTER_MANDATORY deprecated;

{
  /*  Deprecated Message options                                                */
}

  ZMQ_SRCFD = 2 deprecated;


{
    /******************************************************************************/
    /*  0MQ socket events and monitoring                                          */
    /******************************************************************************/

    /*  Socket transport events (TCP, IPC and TIPC only)                          */
}
  ZMQ_EVENT_CONNECTED       = $0001;
  ZMQ_EVENT_CONNECT_DELAYED = $0002;
  ZMQ_EVENT_CONNECT_RETRIED = $0004;
  ZMQ_EVENT_LISTENING       = $0008;
  ZMQ_EVENT_BIND_FAILED     = $0010;
  ZMQ_EVENT_ACCEPTED        = $0020;
  ZMQ_EVENT_ACCEPT_FAILED   = $0040;
  ZMQ_EVENT_CLOSED          = $0080;
  ZMQ_EVENT_CLOSE_FAILED    = $0100;
  ZMQ_EVENT_DISCONNECTED    = $0200;
  ZMQ_EVENT_MONITOR_STOPPED = $0400;
  ZMQ_EVENT_ALL             = $FFFF;

  function zmq_socket(P: Pointer; atype: integer) : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_close(s: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_setsockopt(s: Pointer; option: integer; const optval: Pointer;
      optvallen: LongWord) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_getsockopt(s: Pointer; option: integer; optval : Pointer;
      optvallen: LongWord) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_bind(s: Pointer; const addr: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_connect(s: Pointer; const addr: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_unbind(s: Pointer; const addr: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_disconnect(s: Pointer; const addr: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_send(s: Pointer; const buf: Pointer; len: LongWord; flags: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_send_const(s: Pointer; const buf: Pointer; len: LongWord; flags: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_recv(s: Pointer; buf: Pointer; len: LongWord; flags: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_socket_monitor(s: Pointer; const addr: PChar; events: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
    /******************************************************************************/
    /*  Deprecated I/O multiplexing. Prefer using zmq_poller API                  */
    /******************************************************************************/
}
const
  ZMQ_POLLIN = 1 deprecated 'Deprecated I/O multiplexing. Prefer using zmq_poller API';
  ZMQ_POLLOUT = 2 deprecated 'Deprecated I/O multiplexing. Prefer using zmq_poller API';
  ZMQ_POLLERR = 4 deprecated 'Deprecated I/O multiplexing. Prefer using zmq_poller API';
  ZMQ_POLLPRI = 8 deprecated 'Deprecated I/O multiplexing. Prefer using zmq_poller API';

type zmq_pollitem_t = packed record
  socket: Pointer;
{$IFDEF WIN32}
  fd: LongWord;
{$ELSE}
  fd: integer;
{$ENDIF}
  events: Smallint;
  revents: Smallint;
end;

const
  ZMQ_POLLITEMS_DFLT = 16 deprecated 'Deprecated I/O multiplexing. Prefer using zmq_poller API';

  function zmq_poll(var items: zmq_pollitem_t; nitems: integer; timeout: LongInt ) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; deprecated 'Deprecated I/O multiplexing. Prefer using zmq_poller API';
{
  /******************************************************************************/
  /*  Message proxying                                                          */
  /******************************************************************************/
}

  function zmq_proxy(frontend: Pointer; backend: Pointer; capture: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_proxy_steerable(frontend: Pointer; backend: Pointer;
      capture: Pointer; control: Pointer): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /******************************************************************************/
  /*  Probe library capabilities                                                */
  /******************************************************************************/
}
const
  ZMQ_HAS_CAPABILITIES = 1;

  function zmq_has(const capability: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /*  Deprecated aliases */
}
const
  ZMQ_STREAMER = 1 deprecated;
  ZMQ_FORWARDER = 2 deprecated;
  ZMQ_QUEUE = 3 deprecated;

{
  /*  Deprecated methods */
}

  function zmq_device(atype: integer; frontend: Pointer; backend: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; deprecated;
  function zmq_sendmsg(s: Pointer; var msg: zmq_msg_t; flags: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; deprecated;
  function zmq_recvmsg(s: Pointer; var msg: zmq_msg_t; flags: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; deprecated;

type iovec = record end;
  function zmq_sendiov(s: Pointer; iov: iovec; count: LongWord; flags: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; deprecated;
  function zmq_recviov(s: Pointer; iov: iovec; count: LongWord; flags: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; deprecated;

{
  /******************************************************************************/
  /*  Encryption functions                                                      */
  /******************************************************************************/
}

  {
  /*  Encode data with Z85 encoding. Returns encoded data                       */
  }
  function zmq_z85_encode(dest: PChar; const data: PByte; size: LongWord) : PChar; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

  {
  /*  Decode data with Z85 encoding. Returns decoded data                       */
  }
  function zmq_z85_decode(dest: PByte; const astring: PChar) : PByte; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

  {
  /*  Generate z85-encoded public and private keypair with tweetnacl/libsodium. */
  /*  Returns 0 on success.                                                     */
  }
  function zmq_curve_keypair(z85_public_key: PChar; z85_secret_key: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

  {
  /*  Derive the z85-encoded public key from the z85-encoded secret key.        */
  /*  Returns 0 on success.                                                     */
  }
  function zmq_curve_public(z85_public_key: PChar; const z85_secret_key: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /******************************************************************************/
  /*  Atomic utility methods                                                    */
  /******************************************************************************/
}

  function zmq_atomic_counter_new : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  procedure zmq_atomic_counter_set(counter: Pointer; value: integer); cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_atomic_counter_inc(counter: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_atomic_counter_dec(counter: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_atomic_counter_value (counter: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  procedure zmq_atomic_counter_destroy(counter_p: PPointer); cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /******************************************************************************/
  /*  These functions are not documented by man pages -- use at your own risk.  */
  /*  If you need these to be part of the formal ZMQ API, then (a) write a man  */
  /*  page, and (b) write a test case in tests.                                 */
  /******************************************************************************/

  /*  Helper functions are used by perf tests so that they don't have to care   */
  /*  about minutiae of time-related functions on different OS platforms.       */
}
  {
  /*  Starts the stopwatch. Returns the handle to the watch.                    */
  }
  function zmq_stopwatch_start : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

  {
  /*  Stops the stopwatch. Returns the number of microseconds elapsed since     */
  /*  the stopwatch was started.                                                */
  }
  function zmq_stopwatch_stop(watch: Pointer) : {$IFDEF CPU32} LongWord {$ENDIF} {$IFDEF CPU64} QWord {$ENDIF}; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

  {
  /*  Sleeps for specified number of seconds.                                   */
  }
  procedure zmq_sleep(seconds: integer); cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

type zmq_thread_fn = procedure(P: Pointer);

  {
  /* Start a thread. Returns a handle to the thread.                            */
  }
  function zmq_threadstart(func: zmq_thread_fn; arg: Pointer) : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

  {
  /* Wait for thread to complete then free up resources.                        */
  }
  procedure zmq_threadclose(thread: Pointer); cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /******************************************************************************/
  /*  These functions are DRAFT and disabled in stable releases, and subject to */
  /*  change at ANY time until declared stable.                                 */
  /******************************************************************************/

  /*  DRAFT Socket types.                                                       */
}
const
  ZMQ_SERVER = 12 experimental;
  ZMQ_CLIENT = 13 experimental;
  ZMQ_RADIO = 14 experimental;
  ZMQ_DISH = 15 experimental;
  ZMQ_GATHER = 16 experimental;
  ZMQ_SCATTER = 17 experimental;
  ZMQ_DGRAM = 18 experimental;

{
  /*  DRAFT 0MQ socket events and monitoring                                    */
}

  ZMQ_EVENT_HANDSHAKE_FAILED  = $0800 experimental;
  ZMQ_EVENT_HANDSHAKE_SUCCEED = $1000 experimental;

{
  /*  DRAFT Context options                                                     */
}
  ZMQ_MSG_T_SIZE = 6 experimental;

{
  /*  DRAFT Socket methods.                                                     */
}

  function zmq_join(s: Pointer; const group: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; experimental;
  function zmq_leave(s: Pointer; const group: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; experimental;

{
  /*  DRAFT Msg methods.                                                        */
}

  function zmq_msg_set_routing_id(var msg: zmq_msg_t; routing_id: LongWord) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; experimental;
  function zmq_msg_routing_id(var msg: zmq_msg_t) : LongWord; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; experimental;
  function zmq_msg_set_group(var msg: zmq_msg_t; const group: PChar) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; experimental;
  function zmq_msg_group(var msg: zmq_msg_t) : PChar; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF}; experimental;

{
  /******************************************************************************/
  /*  Poller polling on sockets,fd and thread-safe sockets                      */
  /******************************************************************************/
}

type zmq_poller_event_t = packed record
  socket: Pointer;
  {$IFDEF WIN32}
    fd: LongWord;
  {$ELSE}
    fd: integer;
  {$ENDIF}
  user_data: Pointer;
  events: Smallint;
end;

  function zmq_poller_new : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_destroy(poller_p : PPointer): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_add(poller: Pointer; socket: Pointer; user_data: Pointer; events: ShortInt): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_modify(poller: Pointer; socket: Pointer; events: ShortInt): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_remove(poller: Pointer; socket: Pointer): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_wait(poller: Pointer; var event: zmq_poller_event_t; timeout: LongInt): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_wait_all(poller: Pointer; var events: zmq_poller_event_t;
      n_events: integer; timeout: LongInt): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

  {$IFDEF WIN32}
  function zmq_poller_add_fd (poller: Pointer; fd: LongWord; user_data: Pointer; events: ShortInt): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_modify_fd (poller: Pointer; fd: LongWord; events: ShortInt): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_remove_fd (poller: Pointer; fd: LongWord): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  {$ELSE}
  function zmq_poller_add_fd(poller: Pointer; fd: integer; user_data: Pointer; events: ShortInt): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_modify_fd(poller: Pointer; fd: integer; events: ShortInt): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_poller_remove_fd(poller: Pointer; fd: integer): integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  {$ENDIF}

{
  /******************************************************************************/
  /*  Scheduling timers                                                         */
  /******************************************************************************/
}

  type zmq_timer_fn = procedure (timer_id: integer; arg: Pointer);

  function zmq_timers_new : Pointer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_timers_destroy(timers_p: PPointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_timers_add(timers: Pointer; interval: LongWord; handler: zmq_timer_fn; arg: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_timers_cancel(timers: Pointer; timer_id: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_timers_set_interval(timers: Pointer; timer_id: integer; interval: LongWord) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_timers_reset(timers: Pointer; timer_id: integer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_timers_timeout(timers: Pointer) : LongInt; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};
  function zmq_timers_execute(timers: Pointer) : integer; cdecl; external {$IFNDEF ZMQ_STATIC_LINK}libzmq{$ENDIF};

{
  /******************************************************************************/
  /*  GSSAPI socket options to set name type                                    */
  /******************************************************************************/
}

const
  ZMQ_GSSAPI_PRINCIPAL_NAMETYPE = 90;
  ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE = 91;

{
  /*  GSSAPI principal name types                                               */
}

  ZMQ_GSSAPI_NT_HOSTBASED = 0;
  ZMQ_GSSAPI_NT_USER_NAME = 1;
  ZMQ_GSSAPI_NT_KRB5_PRINCIPAL = 2;

implementation

end.
