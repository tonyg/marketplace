# From Functional I/O to Functional Systems Programming

Support code for ICFP submission.

This is a [Racket](http://racket-lang.org/) package containing a
single
[collection](http://docs.racket-lang.org/reference/collects.html),
`marketplace`, which includes

 - the implementation of the `#lang marketplace` language from the
   paper, in
   [`marketplace/`](https://github.com/tonyg/marketplace/tree/master/marketplace/).

 - the echo server example from the paper, in
   [`marketplace/examples/echo-paper.rkt`](https://github.com/tonyg/marketplace/tree/master/marketplace/examples/echo-paper.rkt).

 - the chat server example from the paper, in
   [`marketplace/examples/chat-paper.rkt`](https://github.com/tonyg/marketplace/tree/master/marketplace/examples/chat-paper.rkt).

 - the Haskell, Erlang and Python implementations of the chat server
   from the paper, in
   [`marketplace/examples/chat.hs`](https://github.com/tonyg/marketplace/tree/master/marketplace/examples/chat.hs),
   [`chat.erl`](https://github.com/tonyg/marketplace/tree/master/marketplace/examples/chat.erl),
   and
   [`chat.py`](https://github.com/tonyg/marketplace/tree/master/marketplace/examples/chat.py)
   respectively.

## How to compile and run the code

You will need the latest **prerelease** version of Racket. Any version
newer than or equal to Racket 5.3.3.7 should work. Nightly-build
installers for Racket can be downloaded
[here](http://pre.racket-lang.org/installers/).

Once you have Racket installed, run

    raco pkg install --link `pwd`

from the root directory of the Git checkout to install the package in
your Racket system. (Alternatively, `make link` does the same thing.)
This will make `#lang marketplace` available to programs.

It will take several minutes to compile the code. On my Macbook Air,
it takes around 10 minutes; on my ridiculously fast desktop machine,
it still takes around 2 minutes.

At this point, you may load and run any of the example `*.rkt` files
in the
[`marketplace/examples/`](https://github.com/tonyg/marketplace/tree/master/marketplace/examples/)
directory.

Note that both the echo server and chat server examples do not print
any output on standard output: instead, they simply start running and
silently await TCP connections. Once one of the servers is running, in
a separate window, try `telnet localhost 5999`.

Note also that both the echo server and the chat server use port 5999,
so you cannot run both simultaneously.

## Copyright

Copyright &copy; Tony Garnock-Jones 2010, 2011, 2012, 2013.
