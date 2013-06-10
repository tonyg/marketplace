#lang scribble/manual
@require[racket/include]
@include{prelude.inc}

@title{Examples}

@section[#:tag "echo-server-example"]{TCP echo server}

Here is a complete Marketplace program:

@#reader scribble/comment-reader (racketmod #:file "examples/echo-paper.rkt" marketplace

(observe-publishers (tcp-channel ? (tcp-listener 5999) ?)
  (match-conversation (tcp-channel from to _)
    (on-presence (spawn (echoer from to)))))

(define (echoer from to)
  (transition stateless
    (publisher (tcp-channel to from ?))
    (subscriber (tcp-channel from to ?)
      (on-absence (quit))
      (on-message
       [(tcp-channel _ _ data)
        (send-message (tcp-channel to from data))]))))
)

The top-level @racket[observe-publishers] monitors TCP connections
arriving on port 5999 and @racket[spawn]s a fresh process in response
to each with the help of the auxiliary @racket[echoer] function. The
topic of conversation associated with the each new connection is
parsed (with @racket[match-conversation]) to name the remote
(@racket[from]) and local (@racket[to]) TCP addresses, which are
passed to @racket[echoer] to create the initial state and actions for
the corresponding process. In this case, the process is stateless,
indicated by the special constant @racket[stateless].

Each connection's process watches for incoming data, using
@racket[from] and @racket[to] to configure a @racket[subscriber]. It
also declares its intent to produce outbound TCP data, using
@racket[publisher]. When data arrives, it is echoed back to the remote
peer using the @racket[send-message] operation. Absence notifications
signal disconnection; when the remote peer closes the TCP connection,
the @racket[on-absence] handler issues a @racket[quit] action, which
terminates the connection's process.

@section[#:tag "chat-server-example"]{TCP chat server}

@#reader scribble/comment-reader (racketmod #:file "examples/chat-paper.rkt" marketplace

(spawn-vm
 (at-meta-level
  (observe-publishers (tcp-channel ? (tcp-listener 5999) ?)
    (match-conversation (tcp-channel them us _)
      (on-presence (spawn (chat-session them us)))))))

(define (chat-session them us)
  (define user (gensym 'user))
  (transition stateless
    (listen-to-user user them us)
    (speak-to-user user them us)))

(define (listen-to-user user them us)
  (list
   (at-meta-level
    (subscriber (tcp-channel them us ?)
      (on-absence (quit))
      (on-message
       [(tcp-channel _ _ (? bytes? text))
        (send-message `(,user says ,text))])))
   (publisher `(,user says ,?))))

(define (speak-to-user user them us)
  (define (say fmt . args)
    (at-meta-level
     (send-message
      (tcp-channel us them (apply format fmt args)))))
  (define (announce who did-what)
    (unless (equal? who user)
      (say "~s ~s.~n" who did-what)))
  (list
   (say "You are ~s.~n" user)
   (at-meta-level
    (publisher (tcp-channel us them ?)))
   (subscriber `(,? says ,?)
     (match-conversation `(,who says ,_)
       (on-presence (announce who 'arrived))
       (on-absence  (announce who 'departed))
       (on-message [`(,who says ,what)
                    (say "~a: ~a" who what)])))))
)

@section[#:tag "chat-client-example"]{TCP chat client}

@#reader scribble/comment-reader (racketmod #:file "examples/chat-client.rkt" marketplace
(require racket/port)

;; Usually it's OK to just use display and friends directly.
;; Here we have a console output driver just to show how it's done.
(name-process 'console-output-driver
  (spawn (transition/no-state
           (subscriber (list 'console-output ?)
             (on-message [(list 'console-output item)
                          (printf "~a" item)
                          (void)])))))

(name-process 'console-input-driver
  (spawn (transition/no-state
           (name-endpoint 'input-relay
             (publisher (list 'console-input ?)
               (on-absence
                (send-message (list 'console-output "Connection terminated.\n"))
                (quit))))
           (subscriber (cons (read-line-evt (current-input-port) 'any) ?)
             (on-message
              [(cons _ (? eof-object?))
               (send-message (list 'console-output "Terminating on local EOF.\n"))
               (delete-endpoint 'input-relay)]
              [(cons _ (? string? line))
               (send-message (list 'console-input line))])))))

(name-process 'outbound-connection
  (spawn (let ((local (tcp-handle 'outbound))
               (remote (tcp-address "localhost" 5999)))
           (transition/no-state
             (subscriber (list 'console-input ?)
               (on-absence (quit))
               (on-message
                [(list 'console-input line)
                 (send-message (list 'console-output (format "> ~a \n" line)))
                 (send-message (tcp-channel local remote (string-append line "\n")))]))
             (publisher (tcp-channel local remote ?))
             (subscriber (tcp-channel remote local ?)
               (on-absence (quit))
               (on-message
                [(tcp-channel _ _ (? eof-object?))
                 (quit)]
                [(tcp-channel _ _ data)
                 (send-message (list 'console-output (format "< ~a" data)))]))))))
)
