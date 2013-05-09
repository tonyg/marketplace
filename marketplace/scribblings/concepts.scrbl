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
messages. See @secref{writing-new-drivers} and @secref{Drivers} for
information on using Racket events from Marketplace programs.

@section{What is a process, what are event handlers?}

A Marketplace @deftech{process} is a collection of event handlers,
plus a piece of private @deftech{process state}. Every
process@note{The exception to this rule is the Ground VM, which plays
a special role.} runs within a containing VM.

When an event occurs that is relevant to a process, one of its event
handlers is called with the process's current state and a description
of the event. The handler is expected to return an updated state value
and a collection of actions for the containing VM to perform. An event
handler, then, has the following approximate type:

@centered{@italic{State} × @italic{Event} → @italic{State} × (Listof @italic{Action})}

@deftech{Events} ...

@deftech{Actions} ...

@section{What is a VM?}

@deftech[#:key "vm"]{Virtual Machines (VMs)} are ...

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

@deftech{Endpoints} are ...

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
  real participants. More on interest-types below.}

]

@deftech{Messages} are ...

@subsection{Topics}

As mentioned above, topics ...

@subsection{Interest Types}

...

@section{Presence}
@section{Nesting, relaying, and levels of discourse}
