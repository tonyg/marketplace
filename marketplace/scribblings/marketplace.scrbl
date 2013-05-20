#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title[#:tag "marketplace"]{Marketplace: Network-Aware Programming}
@;{Marketplace: A Functional Operating System}
@;{Marketplace: A Functional Network Operating System}
@;{Marketplace: A Functional Distributed Operating System}

@author[(author+email "Tony Garnock-Jones" "tonyg@ccs.neu.edu")]

@bold{Every program is a network.}
This is the insight behind the π-calculus. Encoding a program as a
π-calculus term shows it as a network of communicating processes. It
is also one of the original inspirations for Smalltalk, where every
object, every value, was imagined to be a separate computer in a vast
network, and where objects communicated by message-passing.

@bold{Every program is part of a network.}
A program that computes a result but cannot communicate it is useless
indeed. Every complete program both @emph{computes} and
@emph{communicates}. Furthermore, it does so with some finite set of
@emph{resources}, which it must carefully manage.

@bold{Our programming languages do not recognise that every program is
a network.} They blur the distinction between stateful and stateless
portions of a program, making it difficult for programmers to reason
about concurrency, contention, and distribution. They often treat
partial failure as an afterthought, despite its importance in
reasoning about program behaviour, particularly in connection with the
effect of exceptions on stateful programs. They seldom consider issues
of trust and security.

@bold{Our programming languages do not recognise that every program is part of a network.}
They treat communication with the outside world in an ad-hoc manner.
They frequently treat network communication separately from
@tt{1950s-style terminal input and output}. They force the programmer
to divine failures in other parts of the network by arcane means such
as timeouts and examining the entrails of dead communication channels.
They offer no support for allocating or releasing local resources in
response to changes in other parts of the network. They seldom
consider issues of trust and security.

@bold{Marketplace is a network-aware programming language.} As a
corollary, because every program not only computes but also
communicates and manages its resources, Marketplace is also a
distributed operating system.

By recognising that programs communicate both
@emph{internally} (between subprograms) and @emph{externally} (between
peers), we recognise an inherently recursive layered architecture. We
see at every level the @emph{same} concerns of resource management,
location of mutable state, failure detection and recovery, access
control, I/O and user interface, debugging and profiling.

Marketplace addresses these concerns with a small set of primitives
chosen to make network programming in-the-small as flexible, scalable,
manageable and securable as network programming in-the-large---and
vice versa.

@;{
Networks must be manageable. Networks must be monitorable. Networks
must tolerate partial failure. Networks must scale. Networks must
communicate with other networks, via yet other networks.
}

@local-table-of-contents[]

@;@include-section["background.scrbl"]
@include-section["concepts.scrbl"]
@include-section["highlevel.scrbl"]
@include-section["lowlevel.scrbl"]
@include-section["drivers.scrbl"]
@;@include-section["writing-drivers.scrbl"]
@include-section["management-and-monitoring.scrbl"]
@include-section["examples.scrbl"]
