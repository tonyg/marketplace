#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{Concepts}

In this paper, we present a novel "marketplace" approach to
functional systems programming, generalizing
 Felleisen et al.'s "Worlds and Universes" approach to
functional I/O
[ICFP 2009]. We integrate ideas from both distributed systems
and virtualized operating system designs to obtain a novel
architecture of nested virtual machines. Each nested layer is equipped with
its own publish/subscribe network that also propagates information
about the (dis)appearance of services. Our paper presents
several case studies, including a recursive DNS resolver that
has served our lab's DNS traffic for the past nine months.

Our goal is to reconcile this tension, starting from the "Worlds
and Universes" approach to functional I/O and
generalizing to
functional systems programming. We augment
its inherent support for concurrency with publish/subscribe (pub/sub)
messaging, a notion of presence, and
nestable virtual machines (VMs). The result suggests a @emph{
marketplace} metaphor, where communicating programs exist in a
noisy, crowded, even chaotic context, rather than in a quiet place
systematically going through their inboxes.

@section{What is a process, what are event handlers?}

@deftech[#:key "process"]{Processes} are ...

@deftech{Process State} is ...

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
