#lang marketplace
;; Equivalent to chat-paper.rkt, but using the raw unsugared
;; structures rather than the friendly DSL overlay.

(require racket/match)
(require marketplace)

(make-nested-vm
 (lambda (vm-pid)
   (process-spec (lambda (boot-pid)
		   (lambda (k)
		     (k (transition stateless
				    (at-meta-level
				     (add-endpoint 'listener
						   (role 'subscriber
							 (tcp-channel ? (tcp-listener 5999) ?)
							 'observer)
						   listener-event-handler))))))))
 #f)

(define listener-event-handler
  (match-lambda
   [(presence-event (role _ (tcp-channel them us _) _))
    (lambda (state)
      (transition state
		  (spawn (process-spec (lambda (pid) (lambda (k) (k (chat-session them us)))))
			 #f
			 #f)))]
   [_
    (lambda (state) (transition state '()))]))

(define (chat-session them us)
  (define user (gensym 'user))
  (transition stateless
	      (list (listen-to-user user them us)
		    (speak-to-user user them us))))

(define (listen-to-user user them us)
  (list
   (at-meta-level
    (add-endpoint 'tcp-receiver
		  (role 'subscriber
			(tcp-channel them us ?)
			'participant)
		  (match-lambda
		   [(absence-event _ _)
		    (lambda (state)
		      (transition state (quit #f #f)))]
		   [(message-event _ (tcp-channel _ _ (? bytes? text)))
		    (lambda (state)
		      (transition state (send-message `(,user says ,text) 'publisher)))]
		   [_
		    (lambda (state) (transition state '()))])))
   (add-endpoint 'speech-publisher
		 (role 'publisher
		       `(,user says ,?)
		       'participant)
		 (lambda (event)
		   (lambda (state) (transition state '()))))))

(define (speak-to-user user them us)
  (define (say fmt . args)
    (at-meta-level
     (send-message (tcp-channel us them (apply format fmt args))
		   'publisher)))
  (define (announce who did-what)
    (unless (equal? who user)
      (say "~s ~s.~n" who did-what)))
  (list
   (say "You are ~s.~n" user)
   (at-meta-level
    (add-endpoint 'tcp-sender
		  (role 'publisher
			(tcp-channel us them ?)
			'participant)
		  (lambda (event)
		    (lambda (state) (transition state '())))))
   (add-endpoint 'speech-subscriber
		 (role 'subscriber
		       `(,? says ,?)
		       'participant)
		 (match-lambda
		  [(presence-event (role _ `(,who says ,_) _))
		   (lambda (state) (transition state (announce who 'arrived)))]
		  [(absence-event (role _ `(,who says ,_) _) _)
		   (lambda (state) (transition state (announce who 'departed)))]
		  [(message-event _ `(,who says ,what))
		   (lambda (state) (transition state (say "~a: ~a" who what)))]))))
