#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@require[(for-label marketplace/support/spy
		    marketplace/support/debug
		    marketplace/log)]

@title{Management and Monitoring}

@section{generic-spy}

@defmodule[marketplace/support/spy]{

@defproc[(generic-spy [label Any]) Spawn]{

Returns a @racket[spawn] action that, when executed, creates a process
with a @racket[#:subscriber] @racket[endpoint] listening for every
message. Each @racket[EndpointEvent] received by the endpoint is
printed to the current output port. Using this process gives a crude
trace of activity within a VM: @racket[presence-event]s and
@racket[absence-event]s (of @racket[#:publishers]) are logged, as is
each @racket['publisher] message sent to the VM's network.

}

}

@section[#:tag "logging"]{logging (MARKETPLACE_LOG)}

@defmodule*[(marketplace/log-untyped
	     marketplace/log-typed)]{

@defform[#:kind "environment variable" #:id MARKETPLACE_LOG MARKETPLACE_LOG]{

Set the @tt{MARKETPLACE_LOG} environment variable to "debug", "info",
"warning", "error" or "fatal" (i.e. any of Racket's
@racket[log-level?]s) to enable output of log messages at that level
and higher.

If @tt{MARKETPLACE_LOG} is not defined in the environment, @emph{no log
output will be produced}.

}

@defform[(marketplace-log level format-str arg ...)
	 #:grammar
	 ((level expr)
	  (format-str expr)
	  (arg expr))]{

Analogous to Racket's core @racket[log-message], but uses
@racket[marketplace-root-logger] instead of the system logger. The
@racket[level] expression must evaluate to a level symbol (see
@racket[log-level?]), and @racket[format-str] must evaluate to a
format string for use with @racket[format].

}

@defthing[marketplace-root-logger logger?]{

The root logger for marketplace logging.

}

}

@section{debugger (experimental)}

@defmodule[marketplace/support/debug]{

@defproc[(debug [p Spawn]) Spawn]{

Translates a @racket[spawn] action to another spawn action which wraps
the to-be-spawned process in a debugging interface. Executing the
resulting action will not only create a process in the executing VM,
but will also open a debugger GUI.

N.B.: The debugger is experimental and likely to change quite quickly
and unpredictably.

See the file @filepath["examples/debug-chat.rkt"] for an example of the
use of @racket[debug].

}

}
