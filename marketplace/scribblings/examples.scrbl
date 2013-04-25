#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{Examples}

@section{TCP echo Server}

Here is a complete Marketplace program, @tt{examples/echo-paper.rkt}:

@#reader scribble/comment-reader (racketmod marketplace

(endpoint #:subscriber (tcp-channel ? (tcp-listener 5999) ?)
	  #:conversation (tcp-channel from to _)
	  #:on-presence (spawn #:child (echoer from to)))

(define (echoer from to)
  (transition stateless
    (endpoint #:subscriber (tcp-channel from to ?)
	      #:on-absence (quit)
	      [(tcp-channel _ _ data)
	       (send-message (tcp-channel to from data))])))
)

The top-level @racket[endpoint] action subscribes to TCP connections
arriving on port 5999, and @racket[spawn]s a fresh process in response to
each (@racket[#:on-presence]). The topic of
conversation (@racket[#:conversation]) associated with the newly-present
subscription is analyzed to give the remote
(@racket[from]) and local (@racket[to]) TCP addresses, which are
passed to the @racket[echoer] function to give the initial actions for
the corresponding process. Here, the process is stateless, using the
special constant @racket[stateless] as its state.

Each connection's process creates an endpoint subscribing to data
arriving on its particular connection, using @racket[from] and @racket[to]
passed in from the top-level @racket[endpoint]. When data arrives, it is
echoed back to the remote peer using @racket[send-message]. Presence
manages disconnection; when the remote peer closes the TCP connection,
the @racket[#:on-absence] handler in @racket[echoer] issues a @racket[quit]
action, terminating the connection's process. The heart of our system
is the interface between a process and its containing VM. Our
implementation instantiates this interface as a collection of Typed
Racket programs.

@section{TCP chat server}
@section{Authoritative DNS server}
@section{DNS resolver server}
@section{SSH server}
