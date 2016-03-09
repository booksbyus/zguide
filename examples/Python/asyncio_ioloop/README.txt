Python ZeroMQ examples using ioloop and coroutines
====================================================

These example files:

- Have been converted to run under Python 3.

- Use the Python 3 asyncio library.

- Have been modified to satisfy the ``flake8`` syntax and style checker.

- In some cases, use ``asyncio.ensure_future(func_call)``,
  ``loop.run_until_complete(asyncio.wait(tasks))``, and
  ``task.result`` to implement
  parallelism, instead of threads.



.. vim:ft=rst:
