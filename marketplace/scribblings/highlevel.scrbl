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

@section[#:tag "constructing-transitions"]{Constructing transitions}

@deftogether[(
@defform[(transition new-state action-tree ...)]
@defform[(transition: new-state : State action-tree ...)]
@defform[(transition/no-state action-tree ...)]
)]{

Each of these forms produces a @racket[Transition] structure. The
first is for untyped code, the second for typed code (where the
mandatory @racket[State] is the type of the transitioning process's
private state), and the third for either.

Each @racket[action-tree] must be an @racket[(ActionTree State)].

It's fine to include @emph{no} action-trees, in which case the
transition merely updates the state of the process without taking any
actions.

In the case of @racket[transition/no-state], the type @racket[Void]
and value @racket[(void)] is used for the process state.
@racket[transition/no-state] is useful for processes that are
stateless other than the implicit state of their endpoints.

}

@deftogether[(
@defstruct*[transition ([state State] [actions (ActionTree State)]) #:transparent]
@deftype[(Transition State) (transition State)]
)]{

A transition structure. The @racket[transition-state] field is the new
private state the process will have after the transition is applied,
and the @racket[transition-actions] are the actions that will be
performed by the VM in order to apply the transition.

}

@deftogether[(
@deftype[(ActionTree State) (Constreeof (Action State))]
@deftype[(Constreeof X) (Rec CT (U X (Pairof CT CT) False Void Null))]
)]{

An action-tree is a @deftech{cons-tree} of @racket[Action]s. When
performing actions, a VM will traverse an action-tree in left-to-right
order.

@racket['()], @racket[(void)], and @racket[#f] may also be present in
action-trees: when the VM reaches such a value, it ignores it and
continues with the next leaf in the tree.

For example, all of the following are valid action trees which will
send messages @racket[1], @racket[2] and @racket[3] in that order:

@racketblock[(list (send-message 1)
		   (send-message 2)
		   (send-message 3))]

@racketblock[(list (list (send-message 1))
		   (cons (send-message 2) (cons '() (send-message 3))))]

@racketblock[(cons (cons (send-message 1)
			 (send-message 2))
		   (list #f #f (send-message 3)))]

Because @racket[#f] and @racket[(void)] are valid, ignored, members of
an action-tree, @racket[and] and @racket[when] can be used to
selectively include actions in an action-tree:

@racketblock[(list (first-action)
		   (when (condition?)
		     (optional-action))
		   (final-action))]

@racketblock[(list (first-action)
		   (and (condition?)
			(optional-action))
		   (final-action))]

Finally, these inert placeholders can be used to represent "no action
at all" in a transition:

@racketblock[(transition new-state) (code:comment "No action-trees at all")
	     (transition new-state '())
	     (transition new-state (void))
	     (transition new-state #f)]

}

@defproc[(sequence-actions [initial-transition (Transition State)]
			   [item (U (ActionTree State)
				    (State -> (Transition State)))]
			   ...) (Transition State)]{

Returns a transition formed from the @racket[initial-transition]
extended with new actions, possibly updating its carried state. Each
of the supplied @racket[item]s is examined: if it is an
@racket[ActionTree], it is appended to the pending transition's
actions; if it is a procedure, it is called with the @emph{state} of
the pending transition, and is expected to return an updated
transition.

For example,

@racketblock[(sequence-actions (transition 'x
				 (send-message (list 'message 0)))
			       (send-message (list 'message 1))
			       (send-message (list 'message 2))
			       (lambda (old-state)
				 (transition (cons 'y old-state)
				   (send-message (list 'message 3))))
			       (send-message (list 'message 4)))]

produces the equivalent of

@racketblock[(transition (cons 'y 'x)
	       (send-message (list 'message 0))
	       (send-message (list 'message 1))
	       (send-message (list 'message 2))
	       (send-message (list 'message 3))
	       (send-message (list 'message 4)))]

}

@section[#:tag "endpoint-dsl"]{Creating endpoints}

The primitive action that creates new endpoints is
@racket[add-endpoint], but because endpoints are the most flexible and
complex point of interaction between a process and its VM, a DSL,
@racket[endpoint], streamlines endpoint setup.

@deftogether[(
@defform[(endpoint orientation topic maybe-interest-type
		   maybe-let-name
		   maybe-name
		   maybe-state-pattern
		   maybe-on-presence
		   maybe-on-absence
		   maybe-role-patterns
		   maybe-reason-pattern
		   maybe-message-handlers)]
@defform[#:literals (:)
	 (endpoint: maybe-typed-state-pattern : State
		    orientation topic maybe-interest-type
		    maybe-let-name
		    maybe-name
		    maybe-on-presence
		    maybe-on-absence
		    maybe-role-patterns
		    maybe-reason-pattern
		    maybe-message-handlers)
	 #:grammar
	 [(maybe-typed-state-pattern (code:line)
				     (code:line pattern))
	  (orientation #:subscriber
		       #:publisher)
	  (topic expr)
	  (maybe-interest-type (code:line)
			       #:participant
			       #:observer
			       #:everything)
	  (maybe-let-name (code:line)
			  (code:line #:let-name identifier))
	  (maybe-name (code:line)
		      (code:line #:name expr))
	  (maybe-state-pattern (code:line)
			       (code:line #:state pattern))
	  (maybe-on-presence (code:line)
			     (code:line #:on-presence handler-expr))
	  (maybe-on-absence (code:line)
			    (code:line #:on-absence handler-expr))
	  (maybe-role-patterns (code:line)
			       (code:line #:role pattern)
			       (code:line #:peer-orientation pattern
					  #:conversation pattern
					  #:peer-interest-type pattern))
	  (maybe-reason-pattern (code:line)
				(code:line #:reason pattern))
	  (maybe-message-handlers (code:line)
				  (code:line message-handler ...))
	  (message-handler [pattern handler-expr])
	  (handler-expr expr)]]
)]{

Almost everything is optional in an @racket[endpoint]. The only
mandatory parts are the orientation and the topic. For
@racket[endpoint:], the expected type of the process state must also
be supplied.

For example, a minimal endpoint subscribing to all messages would be:

@racketblock[(endpoint #:subscriber ?)]

or in Typed Racket, for a process with @racket[Integer] as its process
state type,

@racketblock[(endpoint: : Integer #:subscriber ?)]

A minimal publishing endpoint would be:

@racketblock[(endpoint #:publisher ?)
	     (endpoint: : Integer #:publisher ?)]

While topic patterns are ordinary Racket data with embedded @racket[?]
wildcards (see @secref{messages-and-topics}), all the other patterns
in an @racket[endpoint] are @racket[match]-patterns. In particular
note that @racket[?] is a wildcard in a topic pattern, while
@racket[_] is a wildcard in a @racket[match]-pattern.

@subsection{Receiving messages}

Supply one or more @racket[message-handler] clauses to handle incoming
message events (as distinct from presence- or absence-events).

The following endpoint @emph{subscribes} to all messages, but only
@emph{handles} some of them:

@racketblock[(endpoint #:subscriber ?
		       ['ping (send-message 'pong)]
		       ['hello (list (send-message 'goodbye)
				     (quit))])]

@subsection{Action-only vs. State updates}

If @racket[#:state] occurs in an @racket[endpoint], or the
@racket[maybe-typed-state-pattern] occurs in an @racket[endpoint:],
then all the @racket[handler-expr]s in that endpoint are expected to
return @seclink["constructing-transitions"]{transition structures}.

If not, however, the event handler expressions are expected to return
plain @racket[ActionTree]s.

This way, simple endpoints that do not need to examine the process
state, and simply act in response to whichever event triggered them,
can be written without the clutter of threading the process state
value through the code.

For example, a simple endpoint could be written either as

@racketblock[(endpoint #:subscriber 'ping
		       ['ping (send-message 'pong)])]

or, explicitly accessing the endpoint's process's state,

@racketblock[(endpoint #:subscriber 'ping
		       #:state old-state
		       ['ping (transition old-state
				(send-message 'pong))])]

@subsection[#:tag "naming-endpoints"]{Naming endpoints}

Endpoint names can be used to @seclink["updating-endpoints"]{update}
or @seclink["deleting-endpoints"]{delete} endpoints.

If @racket[#:name] is supplied, the given value is used as the name of
the endpoint. If not, a fresh name is created. (At present,
@racket[gensym] is used.)

If @racket[#:let-name] is supplied, the given identifier is bound in
the @racket[handler-expr]s to the name of the endpoint. If not, the
name of the endpoint is inaccessible to the @racket[handler-expr]s.

@subsection{Handling presence and absence events}

Other endpoints (in this or other processes) may have matching topics
and complementary orientations to the current endpoint. When such
endpoints come and go, presence and absence events are generated in
the current endpoint.

By default, no actions are taken on such events, but
@racket[#:on-presence] and @racket[#:on-absence] override this
behaviour.

For example, say process A establishes the following endpoint:

@racketblock[(endpoint #:subscriber 'ping
		       #:on-presence (send-message 'pinger-arrived)
		       #:on-absence  (send-message 'pinger-departed)
		       ['ping (send-message 'pong)])]

Some time later, process B takes the following endpoint-establishing
action:

@racketblock[(endpoint #:publisher 'ping
		       #:let-name ping-endpoint-name
		       #:on-presence
		       (list (endpoint #:subscriber 'pong
				       #:let-name pong-waiter-name
				       ['pong (list (delete-endpoint ping-endpoint-name)
						    (delete-endpoint pong-waiter-name))])
			     (send-message 'ping)))]

The sequence of events will be:

@itemlist[

  @item{Process A's @racket[#:on-presence] handler will run, and the
  @racket['pinger-arrived] message will be sent. At the same
  time,@note{In the current implementation, one happens before the
  other, but it is nondeterministic which is run first.} process B's
  @racket[#:on-presence] handler runs, installing a second endpoint
  and sending the @racket['ping] message.}

  @item{Process A's endpoint receives the @racket['ping] message, and
  sends the @racket['pong] message.}

  @item{Process B's second endpoint receives the @racket['pong]
  message, and deletes both of process B's endpoints.}

  @item{The @racket[#:on-absence] handler in process A runs, sending
  the @racket['pinger-departed] message.}

#:style 'ordered]

One possible trace of messages in the VM containing processes A and B is

@racketblock['pinger-arrived
	     'ping
	     'pong
	     'pinger-departed]

By sending the @racket['ping] message @emph{only} once the
@racket[#:on-presence] handler has fired, process B ensures that
someone is listening for pings.

This way, if process B starts before process A, then B will
automatically wait until A is ready to receive ping requests before
issuing any.

@subsection{Exit reasons}

If a @racket[#:reason] pattern is supplied, then the exit reason
supplied to the @racket[delete-endpoint] or @racket[quit] action that
led to the @racket[absence-event] is available to the endpoint's
@racket[#:on-absence] handler expression.

@subsection[#:tag "updating-endpoints"]{Updating endpoints}

If, when an endpoint is created, an existing endpoint with an
@racket[equal?] name is already present, then if the existing and
to-be-added endpoints have exactly equal roles (meaning equal
orientations, interest-types, and topic patterns), the @emph{handlers}
for the endpoint are @emph{updated} without emitting presence or
absence notifications.

This dubious feature can be used to avoid "glitching" of presence
signals. A future release of this library will include better
automatic support for avoiding such transients.

@subsection{Who am I talking to?}

If either @racket[#:role] or any of @racket[#:peer-orientation],
@racket[#:conversation], or @racket[#:peer-interest-type] are
supplied, the @racket[handler-expr]s are given access to the role
carried in the @racket[EndpointEvent] that triggered them.

This role describes the @emph{intersection of interests} between the
current endpoint and the peer endpoint, and so can proxy for the
identity of the other party. It is in a sense a description of the
scope of the current conversation.

Using @racket[#:role] allows a handler complete access to the
@racket[role] structure in the triggering event. It is more common
however to simply use @racket[#:conversation] to extract the
@racket[role-topic] alone, since it is seldom necessary to examine
@racket[role-orientation] (since it's guaranteed to be complementary
to the orientation of the current endpoint) or
@racket[role-interest-type]. If access to those parts is required, use
@racket[#:peer-orientation] and @racket[#:peer-interest-type].

}

@section[#:tag "deleting-endpoints"]{Deleting endpoints}

@defproc[(delete-endpoint [id Any] [reason Any #f]) Action]{

Use this action to delete a previously-added endpoint by name. The
@racket[delete-endpoint-id] must be @racket[equal?] to the
corresponding @racket[add-endpoint-pre-eid]; when @racket[endpoint]
was used to construct the endpoint to be deleted, the relevant name is
that bound by @racket[#:let-name] or supplied to @racket[#:name]. See
@secref{naming-endpoints}.

If @racket[reason] is supplied, it is included in the corresponding
action, and made available in any resulting @racket[absence-event]s.

}

@section{Sending messages and feedback}

@defproc[(send-message [body Any] [orientation Orientation 'publisher]) Action]{

Constructs a message-sending action with the given orientation.
Usually the correct orientation to use is @racket['publisher]; it
means that the sender of the message is acting in the "publisher"
role. Use @racket['subscriber] instead when acting in the "subscriber"
role, i.e. sending feedback.

}

@defproc[(send-feedback [body Any]) Action]{

Equivalent to @racket[(send-message body 'subscriber)].

}

@section{Creating processes}

@deftogether[(
@defform[(spawn maybe-pid-binding maybe-debug-name maybe-parent-continuation
		#:child boot-expr)]
@defform[#:literals (:)
	 (spawn: maybe-pid-binding maybe-debug-name typed-parent-continuation
		 #:child : ChildStateType boot-expr)
	 #:grammar
	 [(maybe-pid-binding (code:line)
			     (code:line #:pid identifier))
	  (maybe-debug-name (code:line)
			    (code:line #:debug-name expr))
	  (maybe-parent-continuation (code:line)
				     (code:line #:parent k-expr)
				     (code:line #:parent parent-state-pattern k-expr))
	  (typed-parent-continuation (code:line #:parent : ParentStateType)
				     (code:line #:parent : ParentStateType k-expr)
				     (code:line #:parent parent-state-pattern : ParentStateType k-expr))
	  (k-expr expr)
	  (boot-expr expr)]]
)]{

Action describing a new process to create. The @racket[boot-expr]
should be an expression yielding a @racket[transition] that contains
the child process's initial state and initial actions.

If @racket[#:pid] is supplied, the associated identifier is bound to
the child process's PID in both @racket[boot-expr] and the parent's
@racket[k-expr].

Any supplied @racket[#:debug-name] will be used in VM debug output.
See also @secref{logging}.

If @racket[#:parent] is supplied, the associated @racket[k-expr] will
run in the parent process after the child process has been created. If
the @racket[parent-state-pattern] is also supplied, then
@racket[k-expr] must return a @racket[Transition]; otherwise, it must
return an @racket[ActionTree]. Note that in Typed Racket, for type
system reasons, @racket[spawn:] requires @racket[ParentStateType] to
be supplied.

}

@section{Exiting and killing processes}

@defproc[(quit [who (Option PID) #f] [reason Any #f]) Action]{

Action causing the termination of a process. If @racket[who] is
omitted or @racket[#f], terminates the acting process; otherwise,
terminates the peer process having @racket[who] as its PID.

If @racket[reason] is supplied, it is included in the corresponding
action, and made available in any resulting @racket[absence-event]s.

Terminating the current process is as simple as:

@racketblock[(quit)]

When a process raises an exception that it does not catch, its
containing VM catches the exception and turns it into an implicit quit
action. In that case, the @racket[reason] will be the raised exception
itself.

}

@section{Cooperative scheduling}

@deftogether[(
@defform[(yield maybe-state-pattern k-expr)]
@defform[#:literals (:)
	 (yield: typed-state-pattern k-expr)
	 #:grammar
	 [(maybe-state-pattern (code:line)
			       (code:line #:state pattern))
	  (typed-state-pattern (code:line : State)
			       (code:line pattern : State))
	  (k-expr expr)]]
)]{

Lets other processes in the system run for a step, returning to
evaluate @racket[k-expr] only after doing a complete round of the
scheduler.

If @racket[pattern] is supplied, @racket[k-expr] should evaluate to a
@racket[Transition]; otherwise it should produce an @racket[ActionTree].

}

@section{Creating nested VMs}

***** nested-vm, nested-vm:
TODO

@section{Relaying across layers}

***** at-meta-level, at-meta-level:
TODO
