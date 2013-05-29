#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{Concepts}

Marketplace integrates ideas from both distributed systems and
virtualized operating system designs to obtain an architecture of
nested @emph{virtual machines} (VMs). Each nested layer is equipped
with its own publish/subscribe network that also propagates
@emph{presence} information about the (dis)appearance of services.

Throughout this manual, diagrams such as the following will illustrate
various process structures:

@vm-figure[(vm (vm-label "Ground Virtual Machine")
	       (network-label "Ground-level Network Language")
	       (process "A Process")
	       (process "Another Process")
	       (parameterize ((process-height (* 2/3 (process-height)))
			      (vm-height (* 2 (vm-height))))
		 (vm (vc-append (vm-label "Nested VMs are")
				(vm-label "processes too"))
		     (network-label "App-specific language")
		     (process "Process")
		     (process "Process")
		     (process "Process")))
	       (parameterize ((process-height (* 5/4 (process-height))))
		 (process "Yet another process"))
	       (parameterize ((process-height (* 2/3 (process-height))))
		 (vm (vm-label "Another Nested VM")
		     (network-label "Another language")
		     (process "Process")
		     (process "Process"))))]

Rectangular boxes represent VMs. The processes running within each VM
are placed atop its box. The narrow rectangular strip at the top of
each VM's box represents the network connecting all the VM's processes
to each other; it will frequently contain a short description of the
protocols used for communication across the represented network.

A central feature of Marketplace is that VMs are nothing more than
regular processes, making them recursively nestable. Each VM supports
a collection of processes all its own, and its internal IPC medium
carries a VM-specific protocol that is often different from the
protocol spoken by its containing VM.

The outermost VM is called the @emph{ground VM}. The protocol spoken
by processes running within the ground VM is a simple protocol
relating Racket's @tech{synchronizable events} to Marketplace network
messages. See @;{@secref{writing-new-drivers} and} @secref{Drivers} for
information on using Racket events from Marketplace programs.

@section{What is a process, what are event handlers?}

A Marketplace @deftech{process} is a collection of @deftech{event
handlers}, plus a piece of private @deftech{process state}. Every
process@note{The exception to this rule is the Ground VM, which plays
a special role.} runs within a containing VM.

When an event occurs that is relevant to a process, one of its event
handlers is called with the process's current state and a description
of the event. The handler is expected to return an updated state value
and a collection of actions for the containing VM to perform. An event
handler, then, has the following approximate type:

@centered{@italic{State} × @italic{Event} → @italic{State} × (Listof @italic{Action})}

Event handlers are registered with the VM by creating @tech{endpoints}
using the @racket[endpoint] macro (described in @secref{endpoint-dsl}) or
the low-level @racket[add-endpoint] structure (described in
@secref{endpoints-and-messages}).

@deftech{Events}, passed to event handlers, describe the results of
actions from the outside world, neighbouring processes in the VM, or
the VM itself. They are implemented as @racket[struct]s. See
@secref{endpoint-events} for a description of the available event
structures.

@deftech{Actions}, passed back to the VM by event handlers, describe
actions the process wishes to perform. See @secref{Actions} for the
possible actions a process can take.

Note that the result of an event handler function is actually a
@racket[Transition] structure; the actual Typed Racket type of event
handlers is @racket[TrapK], defined in @secref{handler-functions}.

@section{What is a VM?}

@deftech[#:key "vm"]{Virtual Machines (VMs)} are simply a collection
of processes, plus a shared medium of communication that the contained
processes use to communicate with each other. VMs offer access to both
their own @emph{internal} network as well as to the @emph{external}
network owned by the VM's own containing VM.@note{Again, the only
exception here is the Ground VM, which interfaces to the underlying
Racket system and so has no containing VM.}

@section{Endpoints: Subscription and Advertisement}

The Marketplace operating system's inter-process communication
facility is structured around @deftech[#:key
"pub/sub"]{publish/subscribe (pub/sub)} messaging.@note{For a survey
of pub/sub messaging, see
@hyperlink["http://www.cs.ru.nl/~pieter/oss/manyfaces.pdf"]{"The Many
Faces of Publish/Subscribe"}, ACM Computing Surveys, Vol. 35, No. 2,
June 2003, pp. 114–131. There's also plenty out there on the Internet;
a good starting point is to google for
@hyperlink["https://www.google.com/search?q=pub/sub message-oriented middleware"]{pub/sub message-oriented middleware}.}

@deftech{Endpoints} are the representation of a process's engagement
in some protocol. They pair a description of the process's @tech{role}
in a conversation with an @tech{event handler} that responds to events
relating to that role.

A @deftech{role} describes the role some process is playing in a
conversation. Concretely, roles are represented by @racket[Role]
structures. A role can be used by the currently-running process to
describe some role it wishes to play, or can be carried in some
@racket[EndpointEvent] to describe the role some @emph{peer} process
is playing in a conversation.

Roles have three parts:

@itemlist[

  @item{An @deftech{orientation} (type @racket[Orientation]) describes
  whether this role is concerned primarily with @emph{producing} or
  @emph{consuming} messages.}

  @item{A @deftech{topic} is a @deftech{pattern} over messages. Topics
  perform double duty: they both scope conversations and filter
  incoming messages. More on topics below.}

  @item{An @deftech{interest-type} (type @racket[InterestType])
  determines whether the endpoint playing the given role is genuinely
  a participant in matching conversations or is simply observing the
  real participants. See @secref{endpoint-dsl} for more on
  interest-types.}

]

@section[#:tag "messages-and-topics"]{Messages and Topics}

@declare-exporting[marketplace]

@deftech{Messages} are simply Racket data structures. They can be any
value for which @racket[equal?] is defined, any @racket[#:prefab]
structure, most @racket[#:transparent] structures, or any non-object
structure for which @racket[prop:struct-map] can be defined.

As mentioned above, topics are simply patterns over messages. They are
represented as normal data structures @emph{with embedded wildcards}.
Use @racket[?] or @racket[(wild)] to construct a wildcard. For
example, given the following definition,

@racketblock[(struct chat-message (speaker text) #:transparent)]

we can not only create instances that might be used with
@racket[send-message],

@racketblock[(chat-message "Tony" "Hello World!")]

but also create topic patterns using @racket[?]. For example, this
pattern matches anything said by @racket["Tony"]:

@racketblock[(chat-message "Tony" ?)]

This pattern matches chat-messages sent by anyone saying "Hello":

@racketblock[(chat-message ? "Hello")]

And finally, this pattern matches any chat-message at all:

@racketblock[(chat-message ? ?)]

Patterns can be nested. For instance, given the above definition of
@racket[chat-message], the following pattern matches any chat message
greeting anybody at all:

@racketblock[(struct greeting (target) #:transparent)
	     (chat-message ? (greeting ?))]

@section{Presence}

@deftech{Presence} (respectively its opposite, @deftech{absence}) is
an indication that a matching conversational partner exists (resp. no
longer exists) in the network. Presence can be used to synchronize
conversations, setting up a conversational context before messages are
sent.

The term "presence" itself is lifted from Instant Messaging protocols
like XMPP, where it describes the online/offline status of one's chat
buddies. Here, it describes the online/offline status of peer
processes, in terms of which conversations they are willing to engage
in.

The system derives presence information from the set of active pub/sub
subscription and advertisement endpoints a process has created.
Creating a new endpoint with a topic pattern that matches some other
process's endpoint and an orientation @emph{opposite} to the other
process's endpoint causes @racket[presence-event]s to be sent to both
endpoints, informing them of the presence of the other. When a process
crashes, or an endpoint is withdrawn with @racket[delete-endpoint], a
corresponding @racket[absence-event] is sent to the remaining
endpoint.

@section{Nesting, relaying, and levels of discourse}

Because VMs can be nested, and each VM has an IPC network of its own
for the use of its processes, information sometimes needs to be
relayed from a VM's external network to its internal network and vice
versa.

In general, the protocol messages sent across a VM's internal network
may be quite different in syntax and meaning from those sent across
the same VM's external network: consider the case of the
@secref{chat-server-example}, which employs a nested VM to separate
out TCP-related messages from higher-level, application-specific chat
messages:

@vm-figure[(vm (vm-label "Ground VM")
	       (network-label "TCP")
	       (process "TCP driver")
	       (process "TCP listener")
	       (process-space)
	       (process "TCP socket mgr.")
	       (process "TCP socket mgr.")
	       (process-ellipsis)
	       (process-space)
	       (vm (vm-label "Nested VM")
		   (network-label "(X says Y)")
		   (process "Listener")
		   (process-space)
		   (process "Chat session")
		   (process "Chat session")
		   (process-ellipsis)))]

Each VM's network corresponds to a distinct @emph{level of discourse}.
The nesting of VMs is then roughly analogous to the layering of
network protocol stacks. For example (and purely hypothetically!) the
TCP-IP/HTTP/Webapp stack could perhaps be represented as

@vm-figure[(vm (vm-label "Ground VM")
	       (network-label "TCP/IP")
	       (process "TCP driver")
	       (vm (vm-label "HTTP VM")
		   (network-label "HTTP sessions/reqs/reps")
		   (process "HTTP accepter")
		   (vm (vm-label "Session VM")
		       (network-label "Session-specific msgs")
		       (process "App process")
		       (process-ellipsis))
		   (process-ellipsis)))]
