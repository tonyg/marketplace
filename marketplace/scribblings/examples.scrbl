#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{Examples}

@section[#:tag "echo-server-example"]{TCP echo server}

Here is a complete Marketplace program:

@#reader scribble/comment-reader (racketmod #:file "examples/echo-paper.rkt" marketplace

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

@section[#:tag "chat-server-example"]{TCP chat server}

@#reader scribble/comment-reader (racketmod #:file "examples/chat-paper.rkt" marketplace

(nested-vm
 (at-meta-level
  (endpoint #:subscriber (tcp-channel ? (tcp-listener 5999) ?) #:observer
	    #:conversation (tcp-channel them us _)
	    #:on-presence (spawn #:child (chat-session them us)))))

(define (chat-session them us)
  (define user (gensym 'user))
  (transition stateless
	      (listen-to-user user them us)
	      (speak-to-user user them us)))

(define (listen-to-user user them us)
  (list
   (endpoint #:publisher `(,user says ,?))
   (at-meta-level
    (endpoint #:subscriber (tcp-channel them us ?)
	      #:on-absence (quit)
	      [(tcp-channel _ _ (? bytes? text))
	       (send-message `(,user says ,text))]))))

(define (speak-to-user user them us)
  (define (say fmt . args)
    (at-meta-level (send-message (tcp-channel us them (apply format fmt args)))))
  (define (announce who did-what)
    (unless (equal? who user) (say "~s ~s.~n" who did-what)))
  (list
   (say "You are ~s.~n" user)
   (at-meta-level
    (endpoint #:publisher (tcp-channel us them ?)))
   (endpoint #:subscriber `(,? says ,?)
	     #:conversation `(,who says ,_)
	     #:on-presence (announce who 'arrived)
	     #:on-absence  (announce who 'departed)
	     [`(,who says ,what) (say "~a: ~a" who what)])))
)
