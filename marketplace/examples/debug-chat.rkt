#lang marketplace

(require "../support/debug.rkt")

(debug
 (nested-vm
  #:debug-name 'echo
  (at-meta-level
   (endpoint
    #:subscriber (tcp-channel ? (tcp-listener 5999) ?)
    #:observer
    #:conversation (tcp-channel them us _)
    #:on-presence
    (debug
     (spawn #:debug-name (list 'session them)
	    #:child (chat-session them us)))))))

(define (chat-session them us)
  (define user (gensym 'user))
  (transition stateless
    (listen-to-user user them us)
    (speak-to-user user them us)))

(define (listen-to-user user them us)
  (list
   (endpoint #:publisher `(,user says ,?))
   (at-meta-level
    (endpoint #:subscriber (tcp-channel them us ?)
              #:on-absence (quit)
              [(tcp-channel _ _ (? bytes? text))
               (send-message `(,user says ,text))]))))

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
    (endpoint #:publisher (tcp-channel us them ?)))
   (endpoint #:subscriber `(,? says ,?)
     #:conversation `(,who says ,_)
     #:on-presence (announce who 'arrived)
     #:on-absence  (announce who 'departed)
     [`(,who says ,what) (say "~a: ~a" who what)])))
