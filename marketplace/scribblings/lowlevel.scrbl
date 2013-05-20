#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@require[(for-label "../main.rkt")]

@title[#:tag "low-level-interface"]{Low-level interface}

@defmodule[marketplace]

At its heart, the interface between each @tech{process} and its
containing @tech{VM} is based on @tech{handler functions} exchanging
@tech{event} and @tech{action} structures with the VM. Both events and
actions are simple Racket structures.

This low-level interface between a VM and a process is analogous to
the @emph{system call interface} of a Unix-like operating system. The
@secref{high-level-interface} corresponds to the @emph{C library
interface} of a Unix-like operating system.

@section[#:tag "handler-functions"]{Handler Functions}

Each @deftech{handler function} is always associated with a particular
@tech{endpoint}, registered with the VM via
@racket[endpoint]/@racket[endpoint:]/@racket[add-endpoint]. A handler
function for a given process with state type @racket[State] has type:

@racketblock[(EndpointEvent -> State -> (Transition State))]

That is, given an @racket[EndpointEvent] followed by the process's
current state, the handler should reply with a @racket[Transition]
containing a new process state and a collection of @racket[Action]s.

@deftogether[(
@deftype[(Handler State) (TrapK State)]
@deftype[(TrapK State) (EndpointEvent -> (InterruptK State))]
@deftype[(InterruptK State) (State -> (Transition State))]
)]{

Typed Racket types capturing various notions of handler function.

}

@section{Topics and Roles}

@deftype[Topic Any]{

As previously mentioned, @tech{topics} are ordinary Racket values
which may have embedded wildcards.

}

@defthing[? Topic]{

Each time @racket[?] is used in an expression context, it produces a
fresh topic wildcard, suitable for use in a topic pattern.

}

@deftogether[(
@defstruct*[role ([orientation Orientation] [topic Topic] [interest-type InterestType]) #:prefab]
@deftype[Role role]
)]{

Roles are almost always constructed by the
@racket[endpoint]/@racket[endpoint:] macros or by the VM
implementations themselves. User programs generally only need to
destructure @racket[role] instances.

A @racket[role] describes the conversational role of a peer as seen by
some process. For example, a subscriber to topic @racket['foo] with
interest-type @racket['participant] might receive a presence
notification carrying the role

@racketblock[(role 'publisher 'foo 'participant)]

Notice that the orientation of the role is the opposite of the
orientation of the endpoint.

}

@deftype[Orientation (U 'publisher 'subscriber)]{

Describes an endpoint's orientation: will it be acting as a publisher
of messages, or as a subscriber to messages? Publishers (orientation
@racket['publisher]) tend to use @racket[send-message] and tend to
respond to feedback from subscribers; subscribers
(@racket['subscriber]) tend to use @racket[send-feedback] and respond
to messages from publishers.

}

@deftype[InterestType (U 'participant 'observer 'everything)]{

Using interest-type @racket['participant] in an endpoint's role
indicates that the endpoint is intending to act as a genuine
participant in whatever protocol is associated with the endpoint and
its topic.

Using @racket['observer] indicates that the endpoint is intended to
@emph{monitor} other ongoing (participant) conversations instead.
Observer endpoints receive presence and absence notifications about
participant endpoints, but participant endpoints only receive
notifications about other participant endpoints, and not about
observer endpoints.

The @racket['observer] interest-type is intended to make it easier to
monitor resource demand and supply. The monitoring endpoints/processes
can react to changing demand by creating or destroying resources to
match.

Finally, the @racket['everything] interest-type receives notifications
about presence and absence of @emph{all} the types of endpoint,
@racket['participant], @racket['observer], and @racket['everything].
Endpoints with interest-type @racket['everything] are rare: they are
relevant for managing demand for @emph{observers}, as well as in some
cases of cross-layer presence/absence propagation. Most programs (and
even most drivers) will not need to use the @racket['everything]
interest-type.

}

@section[#:tag "endpoint-events"]{Endpoint Events}

@deftogether[(
@deftype[EndpointEvent (U PresenceEvent AbsenceEvent MessageEvent)]
@deftype[PresenceEvent presence-event]
@deftype[AbsenceEvent absence-event]
@deftype[MessageEvent message-event]
)]{

Endpoint events are passed to handler functions by VMs, conveying some
change in the world the process lives in. An endpoint event can signal
the arrival or departure of a conversational peer, or can deliver a
message that has been sent on a VM's IPC facility.

}

@defstruct*[presence-event ([role Role]) #:prefab]{

Indicates the arrival of a new conversational partner: an endpoint
with a topic that intersects our own, with @racket[Orientation]
opposite to our own.

The @racket[presence-event-role] describes the arriving peer, or more
precisely, describes the shared interest between ourselves and the new
peer. In particular, the @racket[role-orientation] of the
@racket[presence-event-role] is the orientation that the @emph{peer}
supplied in its @racket[add-endpoint] structure.

}

@defstruct*[absence-event ([role Role] [reason Any]) #:prefab]{

Indicates the departure of an existing conversational partner, through
either an explicit @racket[delete-endpoint] action or the implicit
deleting of all of a process's endpoints when a process exits.

The @racket[absence-event-role] describes the departing peer,
analogously to @racket[presence-event-role].

}

@defstruct*[message-event ([role Role] [message Message]) #:prefab]{

Indicates the arrival of a message matching the topic pattern in the
handler's @tech{endpoint}.

}

@section{Actions}

@deftogether[(
@deftype[(Action State) (U (PreAction State) (yield State) (at-meta-level State))]
@deftype[(PreAction State) (U (add-endpoint State) delete-endpoint send-message (spawn State) quit)]
)]{

Actions are requests from a process to its containing VM. If wrapped
in an @racket[at-meta-level] structure, the action is to apply to
@emph{the VM's own containing VM}; otherwise, the action applies to
the process's containing VM.

}

@deftogether[(
@defstruct*[at-meta-level ([preaction (PreAction State)]) #:prefab]
@deftype[(AtMetaLevel State) (at-meta-level State)]
)]{

An @racket[at-meta-level] structure wraps a plain action, and makes it
apply to the outer VM instead of the inner VM (the default).

}

@deftogether[(
@defstruct*[yield ([k (InterruptK State)]) #:prefab]
@deftype[(Yield State) (yield State)]
)]{

Because current VM implementations are cooperatively scheduled, it can
sometimes be necessary to explicitly yield the CPU to other processes
using a @racket[yield] action. When control returns to the yielding
process, the @racket[yield-k] is invoked.

}

@subsection[#:tag "endpoints-and-messages"]{Endpoints and Messages}

@deftogether[(
@defstruct*[add-endpoint ([pre-eid Any] [role Role] [handler (Handler State)]) #:prefab]
@deftype[(AddEndpoint State) (add-endpoint State)]
)]{

Creates a new endpoint subscribing to the given @racket[Role]. When
events pertaining to the given role occur, the @racket[Handler] is
invoked.@note{If invoked @racket[at-meta-level], subscribes to events
in the containing VM's container.}

The name of the new endpoint will be the @racket[pre-eid]; it must be
unique within the current process, but otherwise can be any value at
all. If the endpoint's name matches an existing endpoint, and the new
role is the same as the existing endpoint's role, the handler function
is @emph{replaced} in the existing endpoint.

To delete an endpoint, perform a @racket[delete-endpoint] action built
with the name of the endpoint to delete.

}

@deftogether[(
@defstruct*[delete-endpoint ([pre-eid Any] [reason Any]) #:prefab]
@deftype[DeleteEndpoint delete-endpoint]
)]{

Deletes an existing endpoint named @racket[pre-eid]. The given
@racket[reason] is passed along to peer endpoints as part of an
@racket[absence-event].

If no specific reason is needed, it is conventional to supply
@racket[#f] as the @racket[delete-endpoint-reason].

}

@deftogether[(
@defstruct*[send-message ([body Message] [orientation Orientation]) #:prefab]
@deftype[SendMessage send-message]
)]{

Sends a message to peers.@note{Or, if @racket[at-meta-level], peers of
the containing VM.} The given @racket[Orientation] should describe the
role the sender is playing when sending this message: usually, it will
be @racket['publisher], but when the message is @emph{feedback} for
some publisher, it will be @racket['subscriber]. See also
@racket[send-feedback].

}

@subsection{Process Management}

@deftogether[(
@defstruct*[spawn ([spec process-spec] [k (Option (PID -> (InterruptK State)))] [debug-name Any])
		  #:prefab]
@defstruct*[process-spec ([boot (PID -> CoTransition)]) #:prefab]
@deftype[CoTransition
	 (All (Result)
	      (All (State) (Transition State) -> Result)
	      -> Result)]
@deftype[(Spawn State) (spawn State)]
@deftype[ProcessSpec process-spec]
)]{

A @racket[spawn] requests the creation of a sibling process@note{If
wrapped in an @racket[at-meta-level], the new process will instead be
a sibling of the creating process's VM.}. The @racket[spawn-k] runs in
the context of the @emph{creating} process, communicating to it the
PID of the new process.

The @racket[spawn-spec] describes the new process to be created. Its
@racket[process-spec-boot] field is a function taking the PID of the
new process and returning a "cotransition". Cotransitions use a
second-order encoding of existential types to guarantee that the VM
remains oblivious to the specific process state type of the new
process. The downside of this approach is its syntactic and type
complexity: see @racket[spawn:] for an easier-to-use, higher-level
approach.

}

@deftogether[(
@defstruct*[quit ([pid (Option PID)] [reason Any]) #:prefab]
@deftype[Quit quit]
)]{

Kills a sibling process.@note{Or, if @racket[at-meta-level], a sibling
process of the containing VM.} If @racket[quit-pid] is @racket[#f],
kills the current process; otherwise, kills the process with the given
PID. The @racket[quit-reason] is passed on to peers of
currently-active endpoints in the process to be killed, as part of a
@racket[absence-event], just as if each active endpoint were deleted
manually before the process exited.

If no specific reason is needed, it is conventional to supply
@racket[#f] as the @racket[quit-reason].

}

@deftype[PID Number]{

In the current VM implementations, process IDs are simply numbers.
PIDs are scoped to and allocated by each individual VM instance.

}
