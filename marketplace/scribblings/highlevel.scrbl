#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title[#:tag "high-level-interface"]{High-level interface}

@declare-exporting[#:use-sources (marketplace/sugar-values
				  marketplace/sugar-untyped
				  marketplace/sugar-typed)]

This high-level interface between a VM and a process is analogous to
the @emph{C library interface} of a Unix-like operating system. The
@secref{low-level-interface} corresponds to the @emph{system call
interface} of a Unix-like operating system.

@section[#:tag "hashlang-variations"]{Using @tt{#lang marketplace} and friends}

@;{
@defmodulelang*[(marketplace
		 marketplace/flow-control
		 marketplace/typed
		 marketplace/typed/flow-control)]
}

@defmodulelang[marketplace]

Programs written for Marketplace differ from normal Racket modules
only in their selection of language. A Racket module written with
@tt{#lang marketplace}, such as the echo server in
@secref["echo-server-example"], specifies a sequence of definitions
and startup @tech{actions} for an application. Typically, initial
actions spawn application processes and nested VMs, which in turn
subscribe to sources of events from the outside world.

At present, there's just @tt{#lang marketplace}. In future, there will
be a variation for Typed Racket, and languages providing greater
support for flow control, responsibility transfer, and other
networking concepts. For now, Typed Racket programs must be written as
@tt{#lang typed/racket} programs using @racket[(require marketplace)]
and @racket[ground-vm:] explicitly.

@;{
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
}

@section{Using Marketplace as a library}

@defmodule*[(marketplace/sugar-untyped
	     marketplace/sugar-typed)
	    #:use-sources (marketplace/sugar-values
			   marketplace/sugar-untyped
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
(ground-vm tcp
           (subscriber (tcp-channel ? (tcp-listener 5999) ?)
             (match-conversation (tcp-channel from to _)
               (on-presence (spawn (echoer from to))))))
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

@declare-exporting[#:use-sources (marketplace
				  marketplace/sugar-values
				  marketplace/sugar-untyped
				  marketplace/sugar-typed)]

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
complex point of interaction between a process and its VM, a
collection of macros helps streamline endpoint setup.

@deftogether[(
@defform[(publisher topic handler ...)]
@defform[(publisher: State topic handler ...)]
@defform[(subscriber topic handler ...)]
@defform[(subscriber: State topic handler ...)]
@defform[(observe-subscribers topic handler ...)]
@defform[(observe-subscribers: State topic handler ...)]
@defform[(observe-publishers topic handler ...)]
@defform[(observe-publishers: State topic handler ...)]
@defform[(observe-subscribers/everything topic handler ...)]
@defform[(observe-subscribers/everything: State topic handler ...)]
@defform[(observe-publishers/everything topic handler ...)]
@defform[(observe-publishers/everything: State topic handler ...)]
@defform[(build-endpoint pre-eid role handler ...)]
@defform[(build-endpoint: State pre-eid role handler ...)
	 #:grammar
	 [(handler unfiltered-handler
		   (match-state pattern handler ...)
		   (match-orientation pattern handler ...)
		   (match-conversation pattern handler ...)
		   (match-interest-type pattern handler ...)
		   (match-reason pattern handler ...))
	  (unfiltered-handler (on-presence expr ...)
			      (on-absence expr ...)
			      (on-message [pattern expr ...] ...))]]
)]{

The many variations on the core
@racket[build-endpoint]/@racket[build-endpoint:] form exist to give
good control over @racket[InterestType] in the endpoint under
construction;
see @secref{participating-vs-observing}.

Almost everything is optional in an endpoint definition. The only
mandatory part is the topic, unless you're using Typed Racket, in
which case the process state type must also be specified.

For example, a minimal endpoint subscribing to all messages would be:

@racketblock[(subscriber ?)]

or in Typed Racket, for a process with @racket[Integer] as its process
state type,

@racketblock[(subscriber: Integer ?)]

A minimal publishing endpoint would be:

@racketblock[(publisher ?)
	     (publisher: Integer ?)]

While topic patterns are ordinary Racket data with embedded @racket[?]
wildcards (see @secref{messages-and-topics}), all the other patterns
in an endpoint definition are @racket[match]-patterns. In particular
note that @racket[?] is a wildcard in a topic pattern, while
@racket[_] is a wildcard in a @racket[match]-pattern.

@subsection{Receiving messages}

Supply an @racket[on-message] handler clause to an endpoint definition
to handle incoming message events (as distinct from presence- or
absence-events).

The following endpoint @emph{subscribes} to all messages, but only
@emph{handles} some of them:

@racketblock[(subscriber ?
	       (on-message
		['ping (send-message 'pong)]
		['hello (list (send-message 'goodbye)
			      (quit))]))]

@subsection{Action-only vs. State updates}

If a group of handlers is wrapped in @racket[match-state], then all
the wrapped handlers are expected to return
@seclink["constructing-transitions"]{transition structures}.

If not, however, the handler expressions are expected to return plain
@racket[ActionTree]s.

This way, simple handlers that do not need to examine the process
state, and simply act in response to whichever event triggered them,
can be written without the clutter of threading the process state
value through the code.

For example, a simple endpoint could be written either as

@racketblock[(subscriber 'ping
	       (on-message ['ping (send-message 'pong)]))]

or, explicitly accessing the endpoint's process's state,

@racketblock[(subscriber 'ping
	       (match-state old-state
		 (on-message ['ping (transition old-state
				      (send-message 'pong))])))]

@subsection{Handling presence and absence events}

Other endpoints (in this or other processes) may have matching topics
and complementary orientations to the current endpoint. When such
endpoints come and go, presence and absence events are generated in
the current endpoint.

By default, no actions are taken on such events, but
@racket[on-presence] and @racket[on-absence] handlers override this
behaviour.

For example, say process A establishes the following endpoint:

@racketblock[(subscriber 'ping
	       (on-presence (send-message 'pinger-arrived))
	       (on-absence  (send-message 'pinger-departed))
	       (on-message ['ping (send-message 'pong)]))]

Some time later, process B takes the following endpoint-establishing
action:

@racketblock[(let-fresh (ping-endpoint-name pong-waiter-name)
	       (name-endpoint ping-endpoint-name
		 (publisher 'ping
		   (on-presence
		    (list (name-endpoint pong-waiter-name
			    (subscriber 'pong
			      (on-message
			       ['pong (list (delete-endpoint ping-endpoint-name)
					    (delete-endpoint pong-waiter-name))])))
			  (send-message 'ping))))))]

The sequence of events will be:

@itemlist[

  @item{Process A's @racket[on-presence] handler will run, and the
  @racket['pinger-arrived] message will be sent. At the same
  time,@note{In the current implementation, one happens before the
  other, but it is nondeterministic which is run first.} process B's
  @racket[on-presence] handler runs, installing a second endpoint
  and sending the @racket['ping] message.}

  @item{Process A's endpoint receives the @racket['ping] message, and
  sends the @racket['pong] message.}

  @item{Process B's second endpoint receives the @racket['pong]
  message, and deletes both of process B's endpoints.}

  @item{The @racket[on-absence] handler in process A runs, sending
  the @racket['pinger-departed] message.}

#:style 'ordered]

One possible trace of messages in the VM containing processes A and B is

@racketblock['pinger-arrived
	     'ping
	     'pong
	     'pinger-departed]

By sending the @racket['ping] message @emph{only} once the
@racket[on-presence] handler has fired, process B ensures that
someone is listening for pings.

This way, if process B starts before process A, then B will
automatically wait until A is ready to receive ping requests before
issuing any.

@subsection{Exit reasons}

If a handler is wrapped in a @racket[match-reason] form, then the exit
reason supplied to the @racket[delete-endpoint] or @racket[quit]
action that led to the @racket[absence-event] is available to the
endpoint's @racket[on-absence] handler expression.

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

Wrapping a handler in @racket[match-orientation],
@racket[match-conversation], and/or @racket[match-interest-type] gives
a handler access to the contents of the @racket[role] structure
carried in the triggering @racket[EndpointEvent].

The carried role describes the @emph{intersection of interests}
between the current endpoint and the peer endpoint, and so can proxy
for the identity of the other party. It is in a sense a description of
the scope of the current conversation.

It is most common to simply use @racket[match-conversation] to extract
the @racket[role-topic] alone, since it is seldom necessary to examine
@racket[role-orientation] (since it's guaranteed to be complementary
to the orientation of the current endpoint) or
@racket[role-interest-type].

See @secref{Examples} for examples of the use of
@racket[match-conversation] and friends.

@subsection[#:tag "participating-vs-observing"]{Participating in a conversation vs. observing conversations}

The core @racket[build-endpoint] form takes an expression evaluating
to a @racket[role], rather than a simple topic. This gives full
control over the new endpoint's @racket[Orientation] and
@racket[InterestType].

The other forms exist for convenience, since usually the orientation
and interest-type is known statically, and only the topic varies
dynamically:

@itemlist[

  @item{@racket[publisher] and @racket[subscriber] (and typed
variations ending in @tt{:}) are for ordinary @emph{participation} in
conversations;}

  @item{@racket[observe-subscribers] and @racket[observe-publishers]
are for @emph{observing} conversations without participating in them; and}

  @item{@racket[observe-subscribers/everything] and
@racket[observe-publishers/everything] are like the ordinary
@tt{observe-...} variants, but use interest-type @racket['everything]
instead of @racket['observer].}

]

The @racket[publisher], @racket[observe-subscribers] and
@racket[observe-subscribers/everything] forms create
@emph{publisher}-oriented endpoints, and @racket[subscriber],
@racket[observe-publishers] and @racket[observe-publishers/everything]
create @emph{subscriber}-oriented endpoints. The rationale for this is
that as a participant, the code should declare the role being played;
but as an observer, the code should declare the roles being observed.

}

@subsection[#:tag "naming-endpoints"]{Naming endpoints}

Endpoint names can be used to @seclink["updating-endpoints"]{update}
or @seclink["deleting-endpoints"]{delete} endpoints.

@defproc[(name-endpoint [id Any] [add-endpoint-action AddEndpoint]) AddEndpoint]{

Returns a copy of the passed-in @racket[add-endpoint] action
structure, with the @racket[id] field set to the passed-in identifying
value.

}

@defform[(let-fresh (identifier ...) expr ...)]{

Binds the @racket[identifier]s to freshly-gensymmed symbols so that
they are available to the @racket[exprs]. @racket[let-fresh] is useful
for inventing a guaranteed-unused name for a temporary endpoint:

@racketblock[(let-fresh (my-name)
	       (name-endpoint my-name
		 (subscriber ?
		   (on-message [_ (list (delete-endpoint my-name)
					...)]))))]

}

@section[#:tag "deleting-endpoints"]{Deleting endpoints}

@defproc[(delete-endpoint [id Any] [reason Any #f]) Action]{

Use this action to delete a previously-added endpoint by name. The
@racket[id] given must be @racket[equal?] to the corresponding
@racket[add-endpoint-pre-eid]; when @racket[endpoint] was used to
construct the endpoint to be deleted, the relevant name is that bound
by @racket[#:let-name] or supplied to @racket[#:name]. See
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
@defform[(spawn maybe-pid-binding boot-expr)]
@defform[(spawn/continue maybe-pid-binding
			 #:parent parent-state-pattern k-expr
			 #:child boot-expr)]
@defform[#:literals (:)
         (spawn: maybe-pid-binding
		 #:parent : ParentStateType
		 #:child : ChildStateType boot-expr)]
@defform[#:literals (:)
         (spawn/continue: maybe-pid-binding
			  #:parent parent-state-pattern : ParentStateType k-expr
			  #:child : ChildStateType boot-expr)
	 #:grammar
	 [(maybe-pid-binding (code:line)
			     (code:line #:pid identifier))
	  (k-expr expr)
	  (boot-expr expr)]]
)]{

Action describing a new process to create. The @racket[boot-expr]
should be an expression yielding a @racket[transition] that contains
the child process's initial state and initial actions.

If @racket[#:pid] is supplied, the associated identifier is bound to
the child process's PID in both @racket[boot-expr] and the parent's
@racket[k-expr].

The @racket[spawn/continue] and @racket[spawn/continue:] variations
include a @racket[k-expr], which will run in the parent process after
the child process has been created. Note that @racket[k-expr] must
return a @racket[Transition], since @racket[parent-state-pattern] is
always supplied for these variations.

In Typed Racket, for type system reasons, @racket[spawn:] and
@racket[spawn/continue:] require @racket[ParentStateType] to be
supplied as well as @racket[ChildStateType].

}

@defproc[(name-process [id Any] [spawn-action Spawn]) Spawn]{

Returns a copy of the passed-in @racket[spawn] action structure, with
the @racket[debug-name] field set to the passed-in identifying value.
The debug name of a process is used in VM debug output. See also
@secref{logging}.

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
@defform[(yield state-pattern k-expr)]
@defform[#:literals (:)
	 (yield: state-pattern : State k-expr)]
)]{

Lets other processes in the system run for a step, returning to
evaluate @racket[k-expr] only after doing a complete round of the
scheduler.

The state of the yielding process will be matched against
@racket[state-pattern] when the process is resumed, and
@racket[k-expr] must evaluate to a @racket[Transition].

}

@section{Creating nested VMs}

@deftogether[(
@defform[(spawn-vm maybe-vm-pid-binding maybe-boot-pid-binding
		   maybe-initial-state
		   maybe-debug-name
		   boot-action-expr ...)]
@defform[#:literals (:)
	 (spawn-vm: : ParentStateType
		    maybe-vm-pid-binding maybe-boot-pid-binding
		    maybe-typed-initial-state
		    maybe-debug-name
		    boot-action-expr ...)
	 #:grammar
	 [(maybe-vm-pid-binding (code:line)
				(code:line #:vm-pid identifier))
	  (maybe-boot-pid-binding (code:line)
				  (code:line #:boot-pid identifier))
	  (maybe-initial-state (code:line)
			       (code:line #:initial-state expr))
	  (maybe-typed-initial-state (code:line)
				     (code:line #:initial-state expr : StateType))
	  (maybe-debug-name (code:line)
			    (code:line #:debug-name expr))
	  (boot-action-expr expr)]]
)]{

Results in a @racket[spawn] action that starts a nested VM. The
primordial process in the new VM executes the boot-actions with the
given initial state. (If no initial state is supplied, @racket[(void)]
is used.)

If @racket[#:vm-pid] is present, the corresponding identifier is bound
in the boot-action expressions to the container-relative PID of the
new VM itself. If @racket[#:boot-pid] is present, however, the
corresponding identifier is bound to the new-VM-relative PID of the
primordial process in the new VM.

}

@section{Relaying across layers}

@deftogether[(
@defform[(at-meta-level: StateType preaction ...)]
@defproc[(at-meta-level [preaction (PreAction State)] ...) (Action StateType)]
)]{

Each VM gives its processes access to two distinct IPC facilities: the
@emph{internal} one, provided for the VM's processes to talk amongst
themselves, and the @emph{external} one, the network that the VM
itself is a process within.

Marketplace's actions can apply to either of those two networks. By
default, actions apply to the VM of the acting process directly, but
using @racket[at-meta-level] (or @racket[at-meta-level:] in typed
code) to wrap an action @emph{level-shifts} the action to make it
apply at the level of the acting process's VM's container instead.

For example, wrapping an @racket[endpoint] in @racket[at-meta-level]
adds a subscription to the VM's container's network. Instead of
listening to sibling processes of the acting process, the new endpoint
will listen to sibling processes of the acting process's VM. In this
example, the primordial process in the nested VM creates an
endpoint in the VM's own network, the ground VM:

@racketblock[
(spawn-vm
 (at-meta-level
  (subscriber (tcp-channel ? (tcp-listener 5999) ?) ...)))
]

In this example, a new process is spawned as a sibling of the
nested VM rather than as a sibling of its primordial process:

@racketblock[
(spawn-vm
 (at-meta-level
  (spawn (transition/no-state (send-message 'hello-world)))))
]

Compare to this example, which spawns a sibling of the
nested VM's primordial process:

@racketblock[
(spawn-vm
 (spawn (transition/no-state (send-message 'hello-world))))
]

}
