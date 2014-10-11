# Examples in Clojure

See LICENSE in examples directory.

It is assumed that you already have [leiningen](http://leiningen.org/) (v2) installed.
If that's not the case it is highly recommended that you do.

## The lazy way

If you want to get started right away instead of going through the
process of installing libzmq, jzmq etc, there's the option of running the
examples using the excellent [jeromq](https://github.com/zeromq/jeromq) (a pure Java
implementation) instead:

```shell
lein jex <example-name> [args]
```

**Note:** jeromq is currently based on libzmq 3.2.4, so examples using more
recent features will not work using this approach.

## The real&trade; way

If you want to run the examples using the actual libzmq library under the hood,
you first need to install libzmq and jzmq:

OSX:
```shell
brew install zeromq
```

Debian/Ubuntu:
```shell
sudo apt-get install libzmq-dev
```

Install [jzmq](https://github.com/zeromq/jzmq):

```shell
git clone https://github.com/zeromq/jzmq.git
cd jzmq
./autogen.sh
./configure
make
sudo make install
```

The most recent installation information is available at the jzmq README.


Once everything is set up, you can run examples using lein:

```shell
lein ex <example-name> [args]
```

**Caveat:** when running several processes (eg server and client), it should of
course be possible to run one using `lein ex` while running the other using `lein jex`. However, there are some subtle differences between jeromq and libzmq (e.g. "ipc://") that prohibit successful interop in some cases (see jeromq readme).