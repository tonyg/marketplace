#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{Drivers}

@section{event-relay}

@defmodule[marketplace/drivers/event-relay]{

@defproc[(event-relay [self-id Symbol]) Spawn]{

Lets processes in some @racket[nested-vm] interact with the outside
world using @racket[ground-vm]-level event-based subscriptions.

Returns a @racket[spawn] which starts an event-relay process with
debug-name @racket[`(event-relay ,self-id)].

The relay process observes subscriptions matching the topic-pattern
@racket[(cons (? evt?) _)], and when one appears, constructs an
analogous one using @racket[at-meta-level] to connect to the next VM
down the stack. Messages from the meta-level will be relayed up to the
current level. When the subscription disappears, the relay withdraws
the subscription at the meta-level as well.

}

}

@section{tcp-bare}

@defmodule[marketplace/drivers/tcp-bare]{

This module is only available for use by untyped Racket processes. It
is included by default in programs using @tt{#lang marketplace}; see
@secref{hashlang-variations} for information on other language
variants.

@defproc[(tcp-driver) Spawn]{

Returns a @racket[spawn] action which starts a TCP driver. The TCP
driver should run either directly in a ground VM, or in a nested VM
with a running @racket[event-relay].

}

}

@subsection{TCP channels}

@defstruct*[tcp-channel ([source TcpAddress]
			 [destination TcpAddress]
			 [subpacket TcpSubPacket]) #:prefab]{

A TCP channel represents a section of a unidirectional TCP flow
appearing on our local "subnet" of the full TCP network, complete with
source, destination and subpacket. Each TCP connection has two such
flows: one inbound (remote-to-local) bytestream, and one outbound
(local-to-remote) bytestream.

}

@deftype[TcpSubPacket (or/c eof-object? bytes?)]{

Packets carried by @racket[tcp-channel] structures are either
end-of-file objects or raw binary data represented as Racket byte
vectors.

}

@subsection{TCP addresses}

@deftype[TcpAddress (or/c tcp-address? tcp-handle? tcp-listener?)]{

A TCP address describes one end of a TCP connection. It can be either

@itemlist[
  @item{a @racket[tcp-address], representing a remote socket;}
  @item{a @racket[tcp-handle], representing a local socket on a kernel-assigned port; or}
  @item{a @racket[tcp-listener], representing a local socket on a user-assigned port.}
]

}

@defstruct*[tcp-address ([host string?]
			 [port (integer-in 0 65535)]) #:prefab]{

Describes a remote half-connection. The @racket[host] part is to be a
string containing either a hostname (e.g. @racket["localhost"]) or an
ASCII representation of an IP address (e.g. @racket["127.0.0.1"]).

}

@defstruct*[tcp-handle ([id any/c]) #:prefab]{

Describes a local half-connection with a kernel-assigned port number.
The port number is not directly accessible; the @racket[id] is used as
a local name for whichever underlying port number ends up being used.

The @racket[id] must be chosen carefully: it is scoped to the local
VM, i.e. shared between processes in that VM, so processes must make
sure not to accidentally clash in handle ID selection. They are also
used in TcpChannel to mean a specific @emph{instance} of a TCP
connection, so if you are likely to want to reconnect individual
flows, use different values for @racket[id].

}

@defstruct*[tcp-listener ([port (integer-in 0 65535)]) #:prefab]{

Describes a local half-connection with a user-assigned port number.
Use this to describe server sockets.

}

@subsection{Opening an outbound connection}

Choose a @racket[tcp-handle], and then create endpoints as follows:

@racketblock[
(let ((local (tcp-handle 'some-unique-value))
      (remote (tcp-address "the.remote.host.example.com" 5999)))
  (transition/no-state
   (endpoint #:publisher (tcp-channel local remote ?))
   (endpoint #:subscriber (tcp-channel remote local ?)
	     [(tcp-channel _ _ (? eof-object?))
	      (code:comment "Handle a received end-of-file object")
	      (transition ...)]
	     [(tcp-channel _ _ (? bytes? data))
	      (code:comment "Handle received data")
	      (transition ...)])))
]

The TCP driver will automatically create an outbound connection in
response to the presence of the endpoints. When the endpoints are
deleted (or the process exits), the TCP driver will notice the absence
and will close the underlying TCP socket.

For a complete example, see @secref{chat-client-example}.

@subsection{Accepting inbound connections}

Choose a port number, and then create an @emph{observer} endpoint as
follows:

@racketblock[
(endpoint #:subscriber (tcp-channel ? (tcp-listener 5999) ?) #:observer
	  #:conversation (tcp-channel them us _)
	  #:on-presence (spawn #:child (chat-session them us)))
]

The use of @racket[#:observer] here indicates that this endpoint isn't
actually interested in exchanging any TCP data; instead, it is
monitoring demand for such exchanges. The TCP driver uses a rare
@racket[#:everything] endpoint to monitor the presence of
@racket[#:observer]s, and creates listening TCP server sockets in
response. When a connection comes in, the TCP driver spawns a manager
process which offers regular @racket[#:participant] endpoints for
communicating on the newly-arrived socket.

To illustrate the code for handling a newly-arrived connection,

@racketblock[
(define (chat-session them us)
  (transition/no-state
   (endpoint #:subscriber (tcp-channel them us ?)
	     #:on-absence (quit)
	     [(tcp-channel _ _ (? bytes? data))
	      (code:comment "Handle incoming data")
	      (transition ...)])))
]

@subsection{Receiving data}

TCP-related messages will be of the form

@racketblock[(tcp-channel remote-address local-address subpacket)]

where the @racket[subpacket] is either @racket[eof] or a
@racket[bytes?].

@subsection{Sending data}

Send data with

@racketblock[(send-message (tcp-channel local-address remote-address subpacket))]

where, as for receiving data, the @racket[subpacket] is either
@racket[eof] or a @racket[bytes?].

@;{
@section{tcp}
Not yet documented.
}

@section{timer (typed and untyped)}

For examples of the use of the timer driver, see uses of
@racket[set-timer] and @racket[timer-expired] in
@hyperlink["https://github.com/tonyg/marketplace-dns/blob/master/network-query.rkt"]{the
Marketplace-based DNS resolver}.

@section{udp (typed and untyped)}

For examples of the use of the UDP driver, see uses of
@racket[udp-packet] etc. in
@hyperlink["https://github.com/tonyg/marketplace-dns/blob/master/tk-dns.rkt"]{the
Marketplace-based DNS resolver}.
