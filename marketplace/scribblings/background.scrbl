#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{Background}

@section{Network programming in the small}

Even the simplest of programs can be seen as a network. This program
sends a message containing @racket[3] and @racket[4] to @racket[+],
and awaits a reply message, which contains @racket[7]:

@interaction[(+ 3 4)]

Subroutines are network services. Here, when the message @racket[3] is
sent to @racket[add-four-to], it delegates to @racket[+]:

@interaction[(define (add-four-to n) (+ n 4))
	     (add-four-to 3)]

@note{In π-calculus encodings of programs, replication and state are
made explicit.} Immutable programs (of which stateless programs are a
special case) are easily able to be @emph{replicated}; programs using
mutable state, however, must be fixed in place. In fact, strictly
speaking, it is the @emph{state} that must be fixed in place, not the
program @emph{manipulating} the state. In the following program, the
mutable @racket[total] variable must not be replicated:

@interaction[(define accumulate
	       (let ((total 0))
		 (lambda (n)
		   (set! total (+ total n))
		   total)))
	     (accumulate 3)
	     (accumulate 4)
	     (accumulate 0)]

It would be a mistake to simply replace each occurrence of
@racket[accumulate] with its definition, since three separate
@racket[total] locations would be created. However, there is no
problem replicating the @racket[lambda] term, so long as
@racket[total] always refers to the same location:

@interaction[(define total 0)
	     ((lambda (n) (set! total (+ total n)) total) 3)
	     ((lambda (n) (set! total (+ total n)) total) 4)
	     ((lambda (n) (set! total (+ total n)) total) 0)]

Programs raise exceptions to signal partial failure:

@interaction[(define accumulate
	       (let ((total 0))
		 (lambda (n)
		   (when (negative? n)
		     (error 'accumulate "n must be non-negative!"))
		   (set! total (+ total n))
		   total)))
	     (accumulate 3)
	     (accumulate -2)]

Programs can handle exceptions to @emph{contain} partial failure:

@interaction[(with-handlers ([exn:fail? (lambda (e) 'ok)])
	       (error 'oh-dear))]

Partial failures interact with shared, mutable state in unpredictable
ways, especially in larger programs that combine stateful subprograms.

@note{For a good overview of PL-based approaches to security and
trust, see Mark S. Miller's
"@hyperlink["http://www.erights.org/talks/thesis/markm-thesis.pdf"]{Robust
composition: Towards a unified approach to access control and
concurrency control}" and Jonathan A. Rees's
"@hyperlink["http://dspace.mit.edu/bitstream/handle/1721.1/5944/AIM-1564.pdf"]{A
Security Kernel Based on the Lambda-Calculus}".} Programmers often
ignore issues of security and trust between subprograms within a
larger program. Most programming languages entirely lack any means of
securely isolating subprograms from each other, leading to predictable
failures. Even memory-safe languages such as Racket, Java and .NET
only offer weak techniques for securely composing mutually suspicious
subprograms. Techniques such as avoiding @emph{ambient authority} and
using @emph{object capabilities} to compose subprograms

TODO ^

@section{Network programming in the large}

Programs which engage in I/O are very obviously part of a network:

@racketblock[(define (greet-user)
	       (define username (read-line))
	       (define greeting (format "Hello, ~a!\n" username))
	       (display greeting)
	       (newline))
	     (greet-user)]

Here, the network has two components: the program, and the user at the
terminal.

But look closely! There is a difference here between the kind of
communication between this program and its @emph{peer} and the
communication @emph{internal} to the program itself. Portions of the
program relay information from the outside world, translating it to
and from an internal representation as they go, while other portions
perform computations on the relayed information.

@vm-figure[(vm (vm-label "Operating System")
	       (network-label "")
	       (process "User")
	       (process-space)
	       (vm (vm-label "Program")
		   (network-label "")
		   (process "Read line")
		   (process "Format greeting")
		   (process "Print greeting")))]
