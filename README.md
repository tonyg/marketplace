# Marketplace: Bringing the Network into the Programming Language

Marketplace is a concurrent language able to express communication,
enforce isolation, and manage resources. Network-inspired extensions
to a functional core represent imperative actions as values, giving
side-effects locality and enabling composition of communicating
processes.

Collaborating programs are grouped within task-specific *virtual
machines* (VMs) to scope their interactions. Conversations between
programs are multi-party (using a publish/subscribe medium), and
programs can easily participate in many such conversations at once.

Marketplace makes *presence* notifications an integral part of
pub/sub. Programs react to presence and absence notifications that
report the comings and goings of their peers. Presence serves to
communicate changes in demand for and supply of services, both within
a VM and across *nested VM layers*. Programs can give up
responsibility for maintaining presence information and for scoping
group communications to their containing VM.

## Documentation

A (draft) manual for Marketplace is available
[here](http://tonyg.github.io/marketplace/).

## The code

This repository contains a [Racket](http://racket-lang.org/) package,
`marketplace`, which includes

 - the implementation of the `#lang marketplace` language, in the
   [top directory](https://github.com/tonyg/marketplace/tree/master/).

 - a TCP echo server example, in
   [`examples/echo-paper.rkt`](https://github.com/tonyg/marketplace/tree/master/examples/echo-paper.rkt).

 - a TCP chat server example, in
   [`examples/chat-paper.rkt`](https://github.com/tonyg/marketplace/tree/master/examples/chat-paper.rkt).

 - Haskell, Erlang and Python implementations of the chat server for comparison, in
   [`examples/chat.hs`](https://github.com/tonyg/marketplace/tree/master/examples/chat.hs),
   [`chat.erl`](https://github.com/tonyg/marketplace/tree/master/examples/chat.erl),
   and
   [`chat.py`](https://github.com/tonyg/marketplace/tree/master/examples/chat.py)
   respectively.

## Compiling and running the code

You will need Racket version 5.3.4.11 or later.

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
[`examples/`](https://github.com/tonyg/marketplace/tree/master/examples/)
directory.

Note that both the echo server and chat server examples do not print
any output on standard output: instead, they simply start running and
silently await TCP connections. Once one of the servers is running, in
a separate window, try `telnet localhost 5999`.

Note also that both the echo server and the chat server use port 5999,
so you cannot run both simultaneously.

## Copyright

Copyright &copy; Tony Garnock-Jones 2010, 2011, 2012, 2013.
