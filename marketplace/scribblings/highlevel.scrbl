#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@require[(for-label (except-in "../sugar-untyped.rkt"
			       transition/no-state)
		    (only-in "../drivers/tcp-bare.rkt" tcp)
		    (except-in "../sugar-typed.rkt"
			       ?))]

@title[#:tag "high-level-interface"]{High-level interface}

This high-level interface between a VM and a process is analogous to
the @emph{C library interface} of a Unix-like operating system. The
@secref{low-level-interface} corresponds to the @emph{system call
interface} of a Unix-like operating system.

@section[#:tag "hashlang-variations"]{Using @tt{#lang marketplace} and friends}

@defmodulelang*[(marketplace
		 marketplace/flow-control
		 marketplace/typed
		 marketplace/typed/flow-control)]

Programs written for Marketplace differ from normal Racket modules
only in their selection of language. A Racket module written with
@tt{#lang marketplace}, such as the echo server in
@secref["echo-server-example"], specifies a sequence of definitions
and startup @tech{actions} for an application. Typically, initial
actions spawn application processes and nested VMs, which in turn
subscribe to sources of events from the outside world.

There are a handful of variant languages to choose from:

@itemlist[

  @item{@racket[marketplace] is for @emph{untyped} programs, and uses
  the @secref{tcp-bare} TCP driver;}

  @item{@racket[marketplace/flow-control] is like
  @racket[marketplace], but uses the flow-controlled @secref{tcp}
  driver;}

  @item{@racket[marketplace/typed] is like @racket[marketplace], but
  for @emph{typed} programs;}

  @item{@racket[marketplace/typed/flow-control] is like
  @racket[marketplace/flow-control], but for typed programs.}

]

@section{Using Marketplace as a library}

@defmodule*[(marketplace/sugar-untyped
	     marketplace/sugar-typed)]

Instead of using Racket's @tt{#lang} feature, ordinary Racket programs
can use Marketplace features by requiring Marketplace modules
directly.

Such programs need to use @racket[ground-vm]/@racket[ground-vm:] to
start the ground-level VM explicitly. They also need to explicitly
start any drivers they need; for example, the file
@filepath{examples/echo-plain.rkt} uses @racket[ground-vm] along with
@racket[tcp] and an initial @racket[endpoint] action:

@racketblock[
(ground-vm
  tcp
  (endpoint #:subscriber (tcp-channel ? (tcp-listener 5999) ?)
	    #:conversation (tcp-channel from to _)
	    #:on-presence (spawn #:child (echoer from to))))
]

@deftogether[(
@defform[(ground-vm maybe-boot-pid-binding maybe-initial-state initial-action ...)]
@defform[(ground-vm: maybe-boot-pid-binding maybe-typed-initial-state initial-action ...)
	 #:grammar
	 [(maybe-boot-pid-binding (code:line)
	 			  (code:line #:boot-pid id))
	  (maybe-initial-state (code:line)
	  		       (code:line #:initial-state expr))
	  (maybe-typed-initial-state (code:line)
	  		       	     (code:line #:initial-state expr : type))
	  (initial-action expr)]]
)]{

Starts the ground VM, in untyped and typed programs, respectively. If
@racket[#:boot-pid] is specified, the given identifier is bound within
the form to the PID of the @emph{primordial process} that performs the
initial actions. If @racket[#:initial-state] is specified (with a
type, for @racket[ground-vm:]), it is used as the initial state for
the primordial process; if it is not supplied, the primordial process
is given @racket[(void)] as its initial state (and @racket[Void] as
its state type).

}

@section{Constructing topics and roles}

As previously mentioned, @tech{topics} are ordinary Racket values
which may have embedded wildcards. The identifier @racket[?] produces
a fresh wildcard value for use in topic patterns.

For example,
TODO

@section{Constructing transitions}
**** transition, transition:, transition/no-state
**** cons-trees of actions; null, false, void; use of (when)
**** sequence-actions

@section{Actions}
**** Communication-related
***** endpoint, endpoint:
***** delete-endpoint
***** send-message
***** send-feedback
**** Process- and scheduling-related
***** spawn, spawn:
***** quit
***** yield, yield:
***** nested-vm, nested-vm:
**** Cross-layer
***** at-meta-level, at-meta-level:
