#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{REMAINDER}

Figure~\ref{vm-interface-types} specifies the framework and its
underlying library via stylized type signatures.@note{The actual
implementation supports secondary features not essential to the
system, such as debug-names for processes and user-accessible process
identifiers. Also, in Typed Racket, we must encode the
existentially-quantified types of Process and Spawn using second-order
polymorphism.}



Additionally, our framework allows the recursive nesting of
marketplaces, thus realizing Dijkstra's vision of a layered,
virtualized operating system. Processes within a layer can themselves
be the substrate for a further layer of sub-processes. Each layer
communicates internally using protocols appropriate to
just that layer. Relay processes translate messages between protocols as they
cross layer boundaries.

Within a marketplace, the appearance or disappearance of a service
becomes an event that affects interested parties. Our architecture
comes with a notion of presence and absence notification
integrated with each nested layer. Using presence, our
architectural framework naturally delimits conversational contexts and
manages associated resources.

While many existing environments use a "mailbox" metaphor, where programs
exchange messages with peers,
real distributed systems do not behave like orderly postal services.
In practice, messages frequently get lost, through corruption and
congestion. Programs engage in multiple simultaneous conversations.
The services a program depends on may be crashed, down for
maintenance, or still going through their startup procedures. An
orderly startup sequence is an impossibility. The system as a whole
frequently cannot be rebooted, existing instead in a state of constant
recovery. Addresses become stale. Demand for services often outstrips
their supply.

The marketplace metaphor implies that such complications are not
problems to be solved anew by each application, but issues that the
programming environment should solve, once and for all.
In this paper we report on initial progress toward this vision.

We take a three-pronged approach to scaling Worlds and Universes to
systems programming. We make Worlds nestable, transform their event
system into a pub/sub network, and integrate
presence and absence notifications. In addition to satisfying
the criteria of Hudak and Sundaresh, the combination of nesting and
presence gives a principled approach to resource management and to
subsystem isolation and composition. Presence gives a flexible
communications topology to each layer in the layered architecture and
provides a clean account of error signalling.

Our design is at heart a @emph{distributed operating system}. This
idea, together with the recent virtualization trend, suggests the
introduction of a @emph{virtual machine (VM)} in which user programs
run. To each VM, we add pub/sub messaging. We escape the constraints
of a hub-and-spoke routing topology by automatically deducing routing tables from
the set of active pub/sub subscriptions.

Basing message routing on active subscriptions in this way has a
pleasant side effect. Our VM notifies processes when routes relevant
to their interests appear or disappear, yielding a generalized form of
@emph{presence}, a concept
originating in more restricted form in instant messaging networks such
as XMPP. Presence notifications are a common, though
often disguised, feature of communications media, but to date have not
received wide attention.

 Our approach separates discrete actions such as spawning new processes
 and sending and receiving messages from more continuous reactions to
 changes in a process' environment, such as arrival of a new service or
 the crashing of a peer.

 To illustrate the idea of presence, consider a widely-used
 Internet-scale pub/sub network: Twitter. Each Twitter user is
 analogous to a process in our system. Following a user is equivalent
 to subscribing to their message stream. The analog of presence
 notification is the email Twitter sends to inform a user of a new
 follower. In some sense, users tailor their message stream to match
 the perceived interests of their followers; similarly, processes in
 our system base their decisions about what to send on @emph{who is
   listening}. Our system goes further in that presence is
 bidirectional, informing processes not only of subscribers matching
 their advertised intent to publish, but also of publishers matching
 their declared interest in receiving messages.

 To illustrate the idea of presence, consider the essence of the
  BitTorrent file-sharing protocol, as it might be implemented in our
  system. A group of processes share a communication space and
  collaborate to ensure all members have a copy of the file being
  shared. Each process advertises the chunks of a file it holds. Peers
  subscribe to chunks they wish to receive. The VM infrastructure
  computes the intersections between advertisements and subscriptions,
  and conveys that routing information to the processes. As processes
  arrive and depart, the subscription set changes, and the routes
  computed from subscriptions indicate changing demand and supply
  levels for blocks within the network. Presence, then, indicates what
  it is profitable to send to whom.

In order to properly encapsulate and isolate groups of processes
collaborating on subtasks within a larger system, we take care to
ensure that the type of our VM kernel program is a subtype of the type
of its processes, which makes our system @emph{recursively
nestable}. A VM instance can be run as a process within another VM. A
layered structure of nested VMs arises, with each VM encapsulating a
group of related processes. A @emph{ground VM} maps events and actions
to real communication with the outside world. Each subsequent layer
translates between its clients above and its substrate below, in a way
similar both to layers in network architectures such as the OSI
model and to the architecture envisaged
in Hoare's quote above.

 Because presence operates both inside and outside a nestable VM, it
 can be used to automatically propagate demand for services across
 layers. Consider a cloud scenario where a single physical machine
 hosts $n$ Linux virtual machines, each of which hosts $m$ socket-based
 services. Using an approach such as @tt{systemd}'s
 socket-activated OS containers, incoming
 connections not only cause processes to be spawned, but cause whole
 virtual machines to be started. Our system achieves the same
 responsiveness to changing demand, while avoiding the manual
 configuration step necessary with @tt{systemd}: presence expressed
 by the innermost processes flows across successive levels of
 containment to the ground VM, where it can be turned into actual
 TCP activity by a TCP driver.

\begin{figure}[t]
  \centering
  \begin{tabular}{|l|c|c|}
    \hline
    Challenge                   & Traditional model & Marketplace model \\
    \hline
    Application logic           & App      & App      \\
    User interface              & App      & App      \\
    Service discovery           & App      & Language \\
    Session lifetime            & App      & Language \\
    Demand tracking             & App      & Language \\
    Fault isolation             & App      & Language \\
    Routing                     & App      & Language \\
    Messaging                   & Language & Language \\
    Concurrency                 & Language & Language \\
    \hline
  \end{tabular}
  \ruledcaption{Challenges faced, and division of responsibility}
  \label{asynchronous-challenges}
\end{figure}

In this way, we have moved from a "mailbox" model based strictly
around producing and consuming messages toward a "marketplace"
model. Figure~\ref{asynchronous-challenges} summarizes the burdens
that our marketplace architecture lifts from applications. Each VM makes a
"bazaar" of interacting vendors and buyers. Groups of collaborating
programs are placed within task-specific VMs to scope their
interactions. Conversations between programs are multi-party, and
programs naturally participate in many such conversations at once. Not
only are messages sent and received, but programs react to presence
notifications that report the comings and goings of their peers.
Presence also serves to communicate changes in demand for and supply
of services, both within a VM and across layers. Programs are no longer
responsible for maintaining presence information or for
scoping group communications; their containing virtual machine takes
on those tasks for them.


@section{Interface}

Our @emph{processes} generalize World programs by replacing the
latter's special-purpose input handlers with @emph{endpoints}, a
single, general construct for handling (possibly message-carrying)
@emph{events}. Existentially-quantified types hide process
states (\State) from the kernel, and we hide kernel state from
processes by never passing it into user code. Given an event and a
current process state, event handlers respond with a
@emph{transition}, which bundles a new process state with a list of
@emph{actions}. The containing VM interprets these action data
structures. Actions can be
communication-related (@racket[add-endpoint] and
@racket[delete-endpoint], @racket[send-message]),
process-related (@racket[spawn], @racket[quit]), or cross-layer
(@racket[at-meta-level]).

A virtual machine groups and isolates a collection of processes; in
turn, it presents itself as a process to another group of processes.
That is, a system consists of nested layers of processes that interact
via messages. The bottom-most (ground) layer is the runtime library of
our language, and interacts with the real world.

\paragraph*{Starting an Application.}

Applications differ from normal Racket modules only in their selection
of language. A Racket module written
with @tt{#lang marketplace}, such as the echo server in
figure~\ref{echo-paper3}, specifies a sequence of definitions and
startup actions for an application. Typically, initial actions spawn
application processes and nested VMs, which in turn subscribe to
sources of events from the outside world.

\paragraph*{Endpoints, Conversations, Messaging and Feedback.}
Processes engage in multiple simultaneous conversations. Each
process therefore has a set of active subscription @emph{endpoints},
each of which selects a subset of the messages on the network. Roughly
speaking, each endpoint plays a @emph{role} within an
ongoing conversation. Publishers and subscribers declare their
interests to their containing VM via @emph{advertisements} and @emph{
subscriptions}, respectively, created with @racket[add-endpoint]
actions:
@#reader scribble/comment-reader (racketblock
(add-endpoint @emph{endpoint-id}
    (role @emph{orientation} @emph{topic} @emph{interest-type})
    (\LAMBDA (event)
     (\LAMBDA (state)
      @emph{... computation resulting in:}
      (transition @emph{new-state} @emph{action0} @emph{action1} ...))))
)
Endpoints are the most complex structure in our system's interface,
and so deserve careful explanation. They are named, for later
reference in @racket[delete-endpoint] actions:
@#reader scribble/comment-reader (racketblock
(delete-endpoint @emph{endpoint-id})
)

 TGJ: This sentence is probably not required?
   Endpoint IDs must be unique within the scope of a process.

\noindent
Endpoints contain a @emph{role}, which generalizes traditional notions
of advertisement and subscription by combining a topic of conversation
with an orientation: @emph{publisher} or @emph{subscriber}. The
topic filter is a pattern over S-expression-shaped
messages@note{In pub/sub terminology,
this is a @emph{content-based filter}.}
expressed as a general datum with embedded wildcards. Choosing this
representation gives both an intuitive pattern language
and, with unification, a conventional operation for computing topic
intersections.

Borrowing an example from the chat server implementation of section~\ref{sec:example},
the following constructs an endpoint advertising intent to
publish@note{This endpoint exists solely to indicate presence to
others, and its event handler therefore ignores incoming events.} on the
"$X$ says $Y$" topic, where $X$ is bound to a user's name
(@racket[me]) and $Y$ is wild (@racket[?]):
@#reader scribble/comment-reader (racketblock
(add-endpoint 'speaker
    (role 'publisher `(,me says ,?) 'participant)
    (\LAMBDA (event) (\LAMBDA (state) (transition state))))
)

Event handlers dispatch on the type of event and current process
state, returning a transition structure for the VM to process. An
endpoint matching @racket['speaker] might be:
@#reader scribble/comment-reader (racketblock
(add-endpoint 'listener
    (role 'subscriber `(,? says ,?) 'participant)
    (\LAMBDA (event)
      (\LAMBDA (state)
        (match event
          [(presence-event arriving-role)
           ...]     @emph{;; describe the arrival of a user}
          [(absence-event departing-role reason)
           ...]     @emph{;; describe the departure of a user}
          [(message-event sender-role `(,who says ,what))
           ...])))) @emph{;; inform the user that }who@emph{ said }what
)
Since the @emph{presence} of processes is as important as exchanging
messages, we include (dis)appearances of processes as essential
events of a conversation alongside regular message deliveries.
Concretely, presence and absence events carry a
VM-computed @racket[role] structure describing the @emph{intersection}
between the advertised interests of the recipient and the appearing or
disappearing peer.

For example, if endpoint "A" takes on the role of subscriber to
topic @racket[(? says ?)], and a peer process creates an endpoint
"B" taking on the role of publisher within the topic @racket[(Bob ?
?)], then the VM sends a presence event to "A" noting that a
publisher on topic @racket[(Bob says ?)] has appeared. Likewise, the
VM informs "B" of a new subscriber on the same topic. Shared topics
of conversation are just the intersections of the topics of the
endpoints viewed as sets of messages.

@defstruct*[send-message ([body any/c]
			  [orientation orientation?])]{
Processes send messages to peers with @racket[send-message] actions.

The optional orientation is by default @racket['publisher], when
@racket[message-body] is intended for matching @racket['subscriber]s.
Because our system enjoys publisher/subscriber symmetry in its
presence notifications and routing tables, @emph{subscribers} may offer
feedback to @emph{publishers}: with @racket[send-message]
orientation @racket['subscriber], messages can flow @emph{upstream} to
processes playing the conversational role of publisher. Feedback
can express flow-control, mode-selection and message
acknowledgement. To illustrate, endpoint "B" from above might take a
transition
@#reader scribble/comment-reader (racketblock
(transition (compute-bob-state)
  (send-message '(Bob says hello) 'publisher)
  (send-message '(Bob goes-to the-shop) 'publisher))
)
Endpoint "A" would receive just the first message, and might give
feedback with
@#reader scribble/comment-reader (racketblock
(transition (compute-alice-state)
  (send-message '(Alice hears (Bob says hello))
                'subscriber))
)

As another example, the chat program in section~\ref{sec:example}
uses such feedback to manage flow-control between the chat process
and the TCP driver.
}

\paragraph*{Participants and Observers.}
The @emph{interest type} given in an endpoint's @racket[role] structure
allows endpoints to monitor interest in some topic of conversation without
offering to participate in such conversations, or equivalently, to monitor
demand for some service without offering to supply or consume that service.

Endpoints with an interest type of @racket['participant] are regular
subscribers, both receiving and causing presence notifications for
matching participant endpoints in the system. Those with
type @racket['observer], however, @emph{receive} presence notifications
about participants but do not @emph{cause} any. Finally, endpoints
using interest type @racket['everything] receive notifications about
all three types of endpoint in the system.

The ability to passively observe other participants in a conversation
naturally supports supervisor processes.
Such supervisors can create and destroy services in response to changes in demand.

 \begin{figure}
   \centering
   \begin{tabular}{|r|c|c|}
     \hline
                & Participant       & Observer          \\
     \hline
     Subscriber & Informed of pubs. & Informed of pubs. \\
                & Acts as listener  &                   \\
     \hline
     Publisher  & Informed of subs. & Informed of subs. \\
                & Acts as speaker   &                   \\
     \hline
   \end{tabular}
   \ruledcaption{Interest types, roles, and presence events.}
   \label{interest-types-in-our-architecture}
 \end{figure}

\paragraph*{Linguistic Simplifications.}
Often, only a subset of the flexibility of @racket[add-endpoint] is
needed. Hence, definitions like that of the @racket['listener] endpoint look
long-winded. For such cases, a small, optional
endpoint creation domain-specific language provides sensible
defaults. The endpoints in figure~\ref{echo-paper3}, for example, are
created using the DSL instead of building @racket[add-endpoint] structures directly.

\begin{figure}
\begin{tabular}{rcl}
$endpoint$ & := & @tt{(endpoint }$orientation$ $topic$ \\
           &    & $\quad\quad\{interest\}$ \\
\\
           &    & $\quad\quad$\{@tt{#:state }$pattern$\} \\
           &    & $\quad\quad$\{@tt{#:conversation }$pattern$\} \\
           &    & $\quad\quad$\{@tt{#:reason }$identifier$\} \\
\\
           &    & $\quad\quad$\{@tt{#:let-name }$identifier$\} \\
           &    & $\quad\quad$\{@tt{#:name }$expr$\} \\
\\
           &    & $\quad\quad$\{@tt{#:on-presence }$handler$\} \\
           &    & $\quad\quad$\{@tt{#:on-absence }$handler$\} \\
\\
           &    & $\quad\quad message\mhyphen handler^*$@tt{)}\\
\\
$orientation$ & := & @tt{#:publisher} $|$ @tt{#:subscriber} \\
\\
$topic$ & := & $expr$ \\
\\
$interest$ & := & @tt{#:participant} $|$ \\
           &    & @tt{#:observer} $|$ \\
           &    & @tt{#:everything} \\
\\
$message\mhyphen handler$ & := & @tt{(}$pattern$ $handler$@tt{)} \\
\\
$handler$ & := & $expr$
\end{tabular}
\ruledcaption{Syntax of the @racket[endpoint] DSL. Braces indicate optional elements; Kleene star indicates repetition.}
\label{endpoint-dsl-syntax}
\end{figure}

 \begin{figure}
 \centering
 \begin{tabular}{|r|c|c|c|c|}
 \hline
 Handler &
   \begin{sideways}@tt{#:state}\end{sideways} &
   \begin{sideways}@tt{#:conversation}\end{sideways} &
   \begin{sideways}@tt{#:reason}\end{sideways} &
   \begin{sideways}@tt{#:let-name}$\quad$\end{sideways} \\
 \hline
 message & \checkmark & \checkmark & & \checkmark \\
 @tt{#:on-presence} & \checkmark & \checkmark & & \checkmark \\
 @tt{#:on-absence} & \checkmark & \checkmark & \checkmark & \checkmark \\
 \hline
 \end{tabular}
 \caption{Scope of bindings in @racket[endpoint] handlers}
 \label{endpoint-dsl-scope}
 \end{figure}

Figure~\ref{endpoint-dsl-syntax} specifies the syntax of the
@racket[endpoint] language. The only mandatory parts of an
@racket[endpoint] are its @emph{orientation}, that is whether it is
a subscription or a publication advertisement, and its @emph{topic}.
Many of the optional clauses introduce new bindings into the scope of
the endpoint's handlers.
 Figure~\ref{endpoint-dsl-scope} summarizes
 the visibility of new bindings in each kind of handler.

With a @racket[#:state] clause, handler expressions can refer to and
update the current process state.
Variables introduced in the associated pattern are scoped over all three types of handler.
If @racket[#:state] is present, handler
expressions are expected to return a full transition structure
including a new process state. If it is absent, however, handler
expressions are expected to return only a list of actions.
This permits concision in the
common case of a stateless process or endpoint. For example, consider
the "no-op" event handler in the @racket['speaker] endpoint example
above. Using @racket[endpoint], it becomes
@#reader scribble/comment-reader (racketblock
(endpoint #:publisher `(,me says ,?))
)

The @racket[#:conversation] clause, again scoped over all handlers,
gives access to the topic of conversation
carried in each notification. The @racket[#:reason] clause, scoped solely over @racket[#:on-absence] handlers, conveys the exit reason code
carried in absence notifications. Endpoint names are introduced
with @racket[#:name], if the program wishes to supply an
explicitly-computed name, or @racket[#:let-name], if programs wish to
delegate name construction to the VM. When @racket[#:let-name] is
used, a guaranteed-fresh endpoint name is supplied to handlers. This permits
an idiom for declaring a temporary endpoint:
@#reader scribble/comment-reader (racketblock
(endpoint #:subscriber some-topic
          #:let-name e
          ;; message handler:
          [request
           (let ([reply (compute-reply request)])
             (list (delete-endpoint e)
                   (send-message reply)))])
)
Message handling clauses at the end of an @racket[endpoint] expression
are run against delivered messages in the usual left-to-right order.
If no clauses match, the delivered message is silently discarded.

\paragraph*{Cross-layer communication.}
Each VM has access to @emph{two} inter-process communication (IPC)
facilities: the external network connecting it to its siblings and
the internal network connecting its contained processes to each other.
When a process hands normal
@racket[add-endpoint], @racket[delete-endpoint] and
@racket[send-message] actions to its VM, they apply to the internal
network of the VM. Actions must be wrapped in an
@racket[at-meta-level] structure to signal to the VM that they are to
apply to the VM's external network.

\begin{figure}[tb]
@#reader scribble/comment-reader (racketblock
(define relay-down
  (endpoint #:subscriber ?
            ;; message handler:
            [message (at-meta-level
                      (send-message message))]))

(define relay-up
  (at-meta-level
   (endpoint #:subscriber ?
             ;; (meta-level) message handler:
             [message (send-message message)])))
)
\ruledcaption{Examples of the use of @racket[at-meta-level]}
\label{at-meta-level-examples}
\end{figure}

Figure~\ref{at-meta-level-examples} demonstrates the use of
@racket[at-meta-level]. Both examples evaluate to
@racket[add-endpoint] action structures. The @racket[relay-down]
endpoint subscribes to the wildcard pattern on the internal network,
and upon receipt of a message, transmits it on the external network.
The @racket[relay-up] endpoint subscribes to the external network and
transmits on the internal network.

Relaying messages between layers is straightforward, but relaying
presence across layers requires the passive @racket['observer] interest-type. An
observer subscription can be used to measure demand for some service
at an upper layer and project it as demand for analogous service at a
lower layer, without appearing to satisfy the upper-layer demand until
matching supply is detected at the lower layer.

\paragraph*{Creating Processes.}
A @racket[spawn] action requests the launch of a new process.
Each @racket[spawn] contains a function producing an initial
transition for the new process:
@#reader scribble/comment-reader (racketblock
(make-spawn
  (\LAMBDA () (transition @emph{state0} @emph{action0} @emph{action1} ...)))
)
The function delays computation of the initial state and initial
actions until the VM installs an appropriate exception handler,
so that blame for any exceptions is correctly apportioned. Because
this is syntactically awkward, a simple shorthand is provided:
@#reader scribble/comment-reader (racketblock
(spawn #:child (transition @emph{state0} @emph{action0} @emph{action1} ...))
)
The VM interpreting the @racket[spawn] datum creates a new process
record with the initial state and queues up the associated actions for
execution. At the type level, a @racket[spawn] action involves a fresh,
existentially-quantified state type variable.

\paragraph*{Exceptions and Process Termination.}

@defstruct*[quit ([pid pid?]
		  [reason any/c])]{
A @racket[quit] action terminates the invoking process, cancelling all
its subscriptions.

The optional @emph{reason code} is passed along to other
processes in any absence notifications arising from the process's
termination. This is analogous to the "exit reason" carried by
Erlang's process exit signals~\cite[\S3.5.6]{Armstrong2003}.

Any exception thrown in an event handler (or during the computation of
an initial transition from a @racket[spawn] action) is caught by the
VM and translated into a @racket[quit] action. This isolates processes, but
not endpoints within processes, from each other's failures.
}

\paragraph*{Scheduling, Management and Monitoring.}
Our current VM implementations cooperatively schedule their processes,
and so support an additional @racket[yield] action, which cedes control
of the CPU to other processes:
@#reader scribble/comment-reader (racketblock
(make-yield (\LAMBDA (state) (transition ...)))
)

 TODO: Check that this is mentioned elsewhere:

 VMs treat processes under their care as linear resources, leaving them
 free to use either a pure-functional approach to managing their state
 or to use side-effecting actions as they see fit.

Finally, many real operating and networking systems
provide reflective facilities which permit listing of running
processes, listing of active network endpoints, killing of processes
by ID, attachment of debuggers to running processes, and so on.
Programmers working with systems that do not provide such facilities
often find themselves implementing makeshift substitutes. Our current
implementation has limited support for such features; we conjecture
that our design will naturally extend to this kind of reflection, but
properly integrating these ideas remains future work.

@section{Implementation}

We have two interworking implementations of our VM
abstraction: one nestable VM used to organize applications, and one
ground VM mapping abstract events to actions in the outside world.

\paragraph*{The Nestable VM.}
The workhorses of our system, nested VM instances are created by a
new linguistic construct, @racket[nested-vm]. Given a list of actions for a primordial
process to run in the new VM, @racket[nested-vm] returns a @racket[spawn]
action that requests the launch of the new VM:
@#reader scribble/comment-reader (racketblock
          (transition @emph{spawner-state}
            (nested-vm @emph{primordial-action} ...))
)
Figure~\ref{spawning-nested-vm} illustrates the creation of a new VM.

\begin{figure}[tb]
@#reader scribble/comment-reader (racketblock
)
  \centering
  \includegraphics[height=3cm]{spawning-nested-vm.eps}
  \ruledcaption{Spawning a nested VM}
  \label{spawning-nested-vm}
\end{figure}

Nested VM instances are implemented as ordinary processes, and so have
state, a state type, and a collection of active subscriptions. Their private
state is nothing more than the table of contained processes:
$$ \State_{vm} = \textrm{PID} \mapsto \textrm{Process} $$
Recall from figure~\ref{vm-interface-types} that the Process type
involves "EPs" and "MetaEPs", which are sets of endpoints
interacting with the VM's internal and external networks,
respectively.

Nested VMs interpret actions from contained processes as they respond
to VM events. Ordinary actions, such as @racket[add-endpoint]
and @racket[spawn], operate on the VM's resources. Meta-level actions,
wrapped in an @racket[at-meta-level] action structure, are translated
into actions that the VM hands back to its container.
Where @racket[spawn] creates a process that is a sibling of the acting
process, an @racket[at-meta-level] @racket[spawn] creates a
process that is a sibling of the VM itself. Similarly, @racket[quit]
can be used with @racket[at-meta-level] to terminate the entire VM,
and @racket[send-message] with @racket[at-meta-level] transmits a
message on the VM's external network, not its internal one.

A meta-level @racket[add-endpoint] action requests the creation of an
endpoint in the @emph{external} network. The VM translates the request
into an action at the VM-as-process level that creates an relaying
endpoint in the @emph{internal} network of the VM's own container. A
record of the relaying "meta-endpoint" is placed in the "MetaEPs"
set of the requesting process, so that when the relaying event handler
fires, the event can be passed to the correct handler in the contained
process. The relaying event handler level-shifts events to compensate
for the level-shifting that took place when the meta-endpoint was
established.

\paragraph*{The Ground VM.}
Virtual machines can only be stacked so far. At some point, they must
connect to the outside world. Our "ground" VM implementation does
just that. Its processes produce real-world output by judicious use of
side-effecting Racket procedures, and await input by using ordinary
subscription endpoints with topics describing Racket's core events.

The ground VM is automatically started for applications written in the
language. Programs written in other languages built on Racket can also
make use of our system by explicitly invoking the @racket[ground-vm]
procedure.

The ground VM monitors subscriptions involving CML-style
event descriptors, interpreting their presence
as demand for the corresponding events and translating them into
concrete I/O requests. When underlying Racket events fire, the
resulting values are sent as messages on the ground VM's internal
network. There, they match subscription topics that caused the event
to be activated in the first place and are delivered to corresponding
endpoints.

Concretely, I/O subscription topic patterns are structured as a pair of a Racket event
descriptor and a pattern matching the values the event yields upon firing.
For example, the timer driver process asks for events when the system
clock advances past a certain point as follows:
@#reader scribble/comment-reader (racketblock
(endpoint
  #:subscriber (cons (timer-evt deadline) ?)
  ;; message handler:
  [(cons \_ current-system-clock-value)
   (begin (display "Time's up!\textbackslashn")
          '())])
)
where @racket[deadline] is the time of the next pending event and
the @racket[timer-evt] function maps such a deadline to an I/O event descriptor.
In the subscription topic, the @racket[car] of the pair is the
event descriptor of interest, and the @racket[cdr] is a wildcard.
In the message-handling pattern, however, the @racket[car] is
ignored since it is simply the event descriptor subscribed to, and
the @racket[cdr] is expected to be the current value of
the system clock. Drivers for other devices construct analogous
subscriptions.

The ground VM is in some ways similar to an @emph{
exokernel} in that it exposes the underlying
"hardware" I/O mechanisms in terms of its own communication
interface. In other words, it multiplexes access to the underlying
system without abstracting away from it.

\paragraph*{Other VM Implementations.}
We have chosen to build our VM implementations in a completely functional
style. Our VM API is deliberately formulated to permit
side-effect-free implementations. Nothing in the interface forces this
choice, however. It is both possible and useful to consider
implementations that internally use imperative features to manage
their process tables, that use Racket's concurrency and parallelism to
improve scalability on multicore machines, that transparently
distribute their contained processes across different machines in a
LAN, and so on.

Because the observable behaviour of a VM is independent of its
implementation, changing the way in which an
application scales may be as simple as switching one
VM implementation for another. We hope to
explore this territory in the future.




\begin{figure}[tb]
@verbatim{
   $ telnet localhost 5999
   Trying 127.0.0.1...
   Connected to localhost.
   Escape character is '^]'.
   You are user63.
   user81 arrived.
   hello
   user63: hello
   user81: hi user63
   user81 departed.
}
\ruledcaption{Transcript of a session with the chat service.}
\label{example-transcript}
\end{figure}
 balance emacs syntax highlighting: $

To illustrate how the pieces of our system fit together, we analyze the source code for a
hub-and-spoke style, TCP-based chat server. The code in this
section is the entirety of the program. Clients connect to the server
with @tt{telnet}. The server assigns a unique name, such as
@tt{user63}, to each connecting client. The arrivals and
departures of peers in the chatroom are announced to connected
clients. Each line of text sent by a client is relayed to every
connected client; figure~\ref{example-transcript} shows a transcript.

Our chat service has two layers, shown in
figure~\ref{chat-service-layering}: a ground layer for the TCP driver
and a nested VM for chats. The latter hosts one process for accepting
incoming connections plus one process per accepted chat connection.
Three types of conversation take place: \circled{1} between the network socket
and its socket manager process; \circled{2} between the socket manager and
its associated chat process; and \circled{3} the multi-party conversation between these
chat processes. Note how each process engages in two distinct conversations
simultaneously.

The server's entry point is a module written in the
@racket[marketplace] language, which automatically starts the ground
VM with the actions given in the module's body:
@#reader scribble/comment-reader (racketblock
;; \ensuremath{\forall\State . \Action{\State}}
(nested-vm
 (at-meta-level
  (endpoint
   #:subscriber (tcp-channel ? (tcp-listener 5999) ?)
   #:observer
   #:conversation (tcp-channel them us _)
   #:on-presence (spawn #:child (chat-session them us))))))
)
This initial action spawns a @racket[nested-vm] to contain
processes specific to our chat
service. Initially, its only process is the primordial process, which
takes on the role of listening for incoming connections.

Recall that each VM has access to two IPC facilities: the external
network of its container and the internal network for its
own processes. The primordial @racket[endpoint] is wrapped in an
@racket[at-meta-level] structure to indicate that it relates to
activity in the VM's external network. Specifically, it is interested
in observing, but not participating in, TCP conversations on local
port number {\tt 5999}. It is this advertisement of interest that @emph{
  implicitly} coordinates with the TCP driver through the presence
mechanism.

\begin{figure}[tb]
  \centering
  \includegraphics[width=6cm]{chat-revised.eps}
  \ruledcaption{Layering and levels of discourse within the chat
    service. Processes started automatically by the system are
    shaded.}
  \label{chat-service-layering}
\end{figure}

The system's TCP driver responds to the appearance of this observer
subscription by creating a listening TCP server socket. When a new TCP
connection arrives, the TCP driver spawns a "socket manager" process
(see figure~\ref{chat-service-layering}) to manage the new socket and
that process creates a subscription for discussing activity on the
socket. The new subscription matches the one shown above in the
listening endpoint. The VM detects the match and sends an
@racket[#:on-presence] notification to the listening endpoint, which
then spawns a process within the App VM whose initial state and
actions are given by @racket[chat-session]:
@#reader scribble/comment-reader (racketblock
;; \TcpAddress\Times\TcpAddress \RArr \Transition{\Stateless}
(define (chat-session them us)
  (define user (gensym 'user))
  (transition stateless
    (listen-to-user user them us)
    (speak-to-user user them us)))
)
The arguments @racket[them] and @racket[us], representing the new
connection's remote and local TCP/IP endpoint addresses, are extracted
from the topic of the conversation that the new peer, the
TCP socket manager process, is willing to have with the chat session:
a conversation about management of a specific TCP
connection.
 No longer true for our simplified case:
 @note{The associated protocol has a lot in common with
   Erlang's I/O protocol,
   \url{http://www.erlang.org/doc/apps/stdlib/io_protocol.html}.}

The initial actions requested by a newly-spawned @racket[chat-session]
are produced by the routines @racket[listen-to-user] and
@racket[speak-to-user]. The @racket[listen-to-user] function
subscribes to incoming TCP data and converts it to messages describing
speech acts, which it then publishes on the internal (nested) network:
@#reader scribble/comment-reader (racketblock
;; \ensuremath{\forall\State. \Symbol\Times\TcpAddress\Times\TcpAddress\rightarrow\Actions{\State}}
(define (listen-to-user user them us)
  (list
   (endpoint #:publisher `(,user says ,?))
   (at-meta-level
    (endpoint #:subscriber (tcp-channel them us ?)
              #:on-absence (quit)
              [(tcp-channel _ _ (? bytes? text))
               (send-message `(,user says ,text))]))))
)
It is the @racket[#:subscriber] endpoint that starts the ongoing
conversation with the TCP socket manager (marked \circled{2} in
figure~\ref{chat-service-layering}). The use of @racket[at-meta-level]
attaches the endpoint to the VM's @emph{external} network, where the domain
of discourse is TCP. The @racket[#:publisher] endpoint, by contrast,
attaches to the @emph{internal} network, where a higher-level chat-specific
protocol is used, and advertises an intent to send chat messages of
the form "$X$ says $Y$."

The presence mechanism appears, for the second time, in
@racket[listen-to-user]. Its @racket[#:on-absence] notification
handler responds to a drop in presence on the topic for the socket's
inbound data stream. This happens when the TCP connection is closed by
the remote @tt{telnet} process; the TCP socket manager process
responds to termination of the TCP connection by @racket[quit]ting. All its
subscriptions are thus deleted, causing matching absence
notifications. In particular, the handler in @racket[listen-to-user]
terminates the chat session process, which causes @emph{its}
subscriptions to be deleted in turn. Thus, changes in presence cascade
through the system along lines determined by the subscriptions of
processes.

The @racket[speak-to-user] function sends a greeting to the user and
then relays events from the internal network to the user via the
connected TCP socket:
@#reader scribble/comment-reader (racketblock
;; \ensuremath{\forall\State. \Symbol\Times\TcpAddress\Times\TcpAddress\rightarrow\Actions{\State}}
(define (speak-to-user user them us)
  \ensuremath{...\textrm{definitions of} say \textrm{and} announce...}
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

\ \\
@#reader scribble/comment-reader (racketblock
;; \ensuremath{\forall\State. \String\Times\Any\Times...\rightarrow\Action{\State}}
(define (say fmt . args)
  (at-meta-level
   (send-message
    (tcp-channel us them (apply format fmt args)))))

;; \ensuremath{\forall\State. \Symbol\Times\Symbol\rightarrow\Action{\State}}
(define (announce who did-what)
  (unless (equal? who user)
    (say "~s ~s.~n" who did-what)))
)
Here we see presence used a third time. In @racket[listen-to-user],
sessions advertise presence as a @emph{publisher} on the "$X$ says
$Y$" topic. This ensures that @emph{subscribers} matching this topic
are informed of the presence of each such publisher. Concretely, when
the publisher endpoint is created, the @racket[#:on-presence]
handlers in @racket[speak-to-user]'s subscriber endpoints in existing
sessions are run. The subscriber endpoint in @racket[speak-to-user]
responds to presence or absence by describing the change to the user.

In sum, a single connection is represented in the system by a
three-party relationship: the remote peer, the TCP socket manager
process, and the chat session process. The remote peer communicates
with the system over TCP as usual (marked \circled{1} in
figure~\ref{chat-service-layering}). The bytes it sends manifest
themselves as Racket-level events on the ground VM's pub/sub network.
The TCP socket manager translates between these low-level events and
the high-level conversational representation of the connection used
with the chat session process (\circled{2} in
figure~\ref{chat-service-layering}).

Each chat session process manages its half of the conversation with
its corresponding TCP socket manager as part of its other
responsibilities. In this case, it relays input from the remote peer
as speech acts on the nested VM's pub/sub network. The other chat
sessions within the nested VM, each one representing the application
side of another TCP connection, subscribe to these relayed speech acts
(\circled{3} in figure~\ref{chat-service-layering}) and format and deliver
them to their remote peers.


According to Hudak and Sundaresh, a

functional I/O system should provide support for
(1) equational reasoning, (2) efficiency, (3) interactivity, (4)
extensibility, and (5) handling of "anomalous situations," or
errors. Broadening our focus to systems programming, we add (6)
resource management and (7) subsystem encapsulation to this list of criteria.

 We have found that the individual elements of our
 approach work well together to address this complex of issues as a whole. Pub/sub
 subscriptions not only permit flexible communications topologies, but
 also give rise to presence information. Presence, in turn, allows resource
 management and crash notification and interacts with our
 nestable VMs to provide encapsulation, isolation, and layering.

\paragraph*{1: Equational Reasoning.}
Like Worlds and Universes, our system allows for equational
reasoning because event handlers are functional state
transducers. When side-effects are absolutely required, they can be
encapsulated in a process, limiting their scope as in our SSH server. The state of the
system as a whole can be partitioned into independent processes,
allowing programmers to avoid global reasoning when designing and unit-testing
their code.

\paragraph*{2: Efficiency.}
Our VM implementations manage both their own state and the state of
their contained processes in a linear way. Hudak and Sundaresh,
discussing their "stream" model of I/O, remark that the state of
their kernel "is a single-threaded object, and so can be implemented
efficiently". Our system shares this advantage with streams.

There are no theoretical obstacles to providing more efficient and
scalable implementations of our core abstractions.
Siena and Hermes both use
subscription and advertisement information to construct efficient
routing @emph{trees}. Using a similar technique for implementing a
virtual machine would permit scale-out of the corresponding
layer without changing any code in the application processes.

\paragraph*{3: Interactivity.}
The term "interactivity" in this context relates to the ability of
the system to interleave communication and computation with other
actors in the system, in particular, to permit user actions to affect
the evolution of the system. Our system
naturally satisfies this requirement because all processes are
concurrently-evolving, communicating entities.

\paragraph*{4: Extensibility.}
Our system is extensible in that our ground VM multiplexes raw Racket
events without abstracting away from them. Hence, driver
processes can be written for our system to adapt it to any I/O
facilities that Racket offers in the future. The collection of
request and response types for the "stream" model given by Hudak and
Sundaresh~\cite[\S 4.1]{Hudak1988} is static and non-extensible
because their operating system is monolithic, with
device drivers baked in to the kernel. On the one hand, monolithicity
means that the possible communication failures are obvious from the
set of device drivers available; on the other hand, its simplistic
treatment of user-to-driver communication means that the system cannot
express the kinds of failures that arise in microkernel or distributed
systems. Put differently, a monolithic stream system is not suitable
for a functional approach to systems programming.

Our action type (figure~\ref{vm-interface-types}) appears to block
future extensions because it consists of a finite set of variants.
This appearance is deceiving. Actions
are merely the interface between a program and its VM.
Extensibility is due to the messages exchanged between a program and
its peers. In other words, the Action type is similar to the limited set of core forms
in the lambda calculus, the limited set of methods in HTTP and the
handful of core system calls in Unix: a finite kernel generating an
infinite spectrum of possibilities.

 a fixed core that can express many other things when combined.

 Protocols such as HTTP and the @tt{9p}
 file-system of Plan 9 take similar approaches: they provide a simple
 protocol with a small number of general-purpose actions which can
 express a wide variety of effects in combination.

\paragraph*{5: Errors.}
In distributed systems, a request can fail in two distinct ways. Some
"failures" are successful communications with a
service, which just happens to fail at some requested
task; but some failures are caused by the unreachability of the
service requested. Our system represents the former kind of failure
via protocols capable of expressing error responses
to requests. For the latter kind of failure, it uses absence
notifications.

\paragraph*{6: Resource Management.}
Presence and absence notifications are also the basis for
resource management in our system. Through the presence mechanism,
programs can measure demand for some resource and allocate or
release it in response.
 There's a really interesting connection to garbage collection here,
 which this comment is too narrow to explain.
Presence arises from considering the intersection of pub/sub topic
filters, but using pub/sub has another benefit. It generalizes
point-to-point, multicast, broadcast and even anycast
communication; the same few primitive actions are able to express any
point along this spectrum. The VM network is responsible
for routing based on interest, decoupling the
language for declaring interest from the semantics of routing.

\paragraph*{7: Subsystem Encapsulation and Isolation.}
Finally, our use of layered, nested VMs encapsulates and
isolates subsystems in a complete program. Our use of a
fixed API between a VM and its processes decouples the implementation
of each layer's virtual machine from its content. We can therefore swap
out one VM implementation for another without altering its processes.

Isolation of process groups is required in a pub/sub system to avoid
potential crosstalk between logically separate groups of processes. In
our system, VMs provide the necessary isolation. If we had chosen
point-to-point communication instead, nesting would not be absolutely
required; however, the use of
pub/sub is a key advantage of our system, since it gives rise to
presence. Presence can be combined with nesting to build
supervision hierarchies that restart entire
nested VM instances in response to failures.


We present a novel approach to functional systems programming,
building on previous work on functional approaches to managing state
and I/O. By incorporating multi-party communications
and explicitly considering concurrency, our model factors out numerous
cross-cutting concerns including discovery, synchronization, failure
detection, and state lifetime. The connection between a process and
its container is declarative. Our model encourages the programmer to
think declaratively, yet in concurrent rather than sequential terms,
writing programs that react smoothly to changes in their environment.

Placing the combination of presence, nested virtualization,
and event-based publish/subscribe communication at the heart of a
system design eliminates a large amount of scattered
application code that recurs across many different kinds of projects.
As a result, programs become smaller and more robust, and programmers
are freed to concentrate on the functionality of their applications.

 Integrating treatment of lost messages, congestion and queue
 management into our approach remains as future work.

\paragraph*{Code.}
The source-code for our system, examples, and case studies is
available at \url{https://github.com/tonyg/marketplace}.
