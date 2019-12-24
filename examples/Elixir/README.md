# ZeroMQ examples for Elixir

## Description

This directory contains Elixir examples.  These Elixir scripts were
created by converting the scripts in `../Erlang` using `erl2ex`
(https://github.com/dazuma/erl2ex.git), and then doing some minor
editing.

In order to run these examples, you will need to build `erlzmq2`
(https://github.com/zeromq/erlzmq2.git).

Then, on Linux, run an example script with something like the
following:

```
$ ERL_LIBS=/my/path/to/erlzmq2 elixir wuserver.exs
```

Or:

```
$ export ERL_LIBS=/my/path/to/erlzmq2
$ elixir wuserver.exs
```

## Conversion status

The files marked "converted" seem to run as expected.

```
asyncsrv.exs -- uses unknown module erlzmq_device
hwclient.exs -- converted
hwserver.exs -- converted
identity.exs -- converted (but, uses unknown module erlzmq_device)
interrupt.exs -- converted
lbbroker.exs -- converted
msgqueue.exs -- uses unknown module erlzmq_device
mspoller.exs -- converted.  Note: Run wuserver.exs.
msreader.exs -- converted.  Note: Run wuserver.exs.
mtrelay.exs
mtserver.exs -- uses unknown module erlzmq_device
psenvpub.exs -- converted
psenvsub.exs -- converted
rrbroker.exs -- converted
rrclient.exs -- converted
rrworker.exs -- converted
rtdealer.exs -- converted
rtreq.exs -- converted
syncpub.exs -- converted.  Note: Run syncpub.exs, then two instances of syncsub.exs.
syncsub.exs -- converted
tasksink2.exs -- converted
tasksink.exs -- converted
taskvent.exs -- converted
taskwork2.exs -- converted
taskwork.exs -- converted
version.exs -- converted
wuclient.exs -- converted
wuproxy.exs -- converted
wuserver.exs -- converted
```

Notes:

* In some cases, I had to convert the use of "*" in an address
  (endpoint) to "localhost".  This seemed to happen, for example, in
  `rrworker.exs` and `rrclient.exs`.

* Several examples use modules `erlzmq_device` or `erlzmq_util`.  I
  was not able to find implementations of these.
