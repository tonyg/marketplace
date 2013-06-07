#lang racket/base
;; TCP drivers, ported from os2.rkt directly, with flow-control and line discipline removed

(require racket/set)
(require racket/match)
(require (prefix-in tcp: racket/tcp))
(require racket/port)
(require "../sugar-untyped.rkt")
(require "../support/dump-bytes.rkt")

(provide (struct-out tcp-address)
	 (struct-out tcp-handle)
	 (struct-out tcp-listener)
	 (struct-out tcp-channel)
	 tcp
	 tcp-driver)

(struct tcp-address (host port) #:prefab)
(struct tcp-handle (id) #:prefab)
(struct tcp-listener (port) #:prefab)

(struct tcp-channel (source destination subpacket) #:prefab)

(define any-remote (tcp-address (wild) (wild)))
(define any-handle (tcp-handle (wild)))
(define any-listener (tcp-listener (wild)))

(define (tcp-driver)
  (name-process 'tcp-driver
    (spawn
     (transition (set)
       (observe-publishers/everything (tcp-channel any-listener any-remote (wild))
	 (match-state active-handles
	   (match-conversation c
	     (on-presence (maybe-spawn-socket 'publisher c active-handles #f tcp-listener-manager))
	     (on-absence (maybe-forget-socket 'publisher c active-handles)))))
       (observe-subscribers/everything (tcp-channel any-remote any-listener (wild))
	 (match-state active-handles
	   (match-conversation c
	     (on-presence (maybe-spawn-socket 'subscriber c active-handles #f tcp-listener-manager))
	     (on-absence (maybe-forget-socket 'subscriber c active-handles)))))
       (observe-publishers (tcp-channel any-handle any-remote (wild))
	 (match-state active-handles
	   (match-conversation c
	     (on-presence
	      (maybe-spawn-socket 'publisher c active-handles #t tcp-connection-manager))
	     (on-absence (maybe-forget-socket 'publisher c active-handles)))))
       (observe-subscribers (tcp-channel any-handle any-remote (wild))
	 (match-state active-handles
	   (match-conversation c
	     (on-presence
	      (maybe-spawn-socket 'subscriber c active-handles #t tcp-connection-manager))
	     (on-absence (maybe-forget-socket 'subscriber c active-handles)))))))))

(define tcp (tcp-driver)) ;; pre-instantiated!

(define (maybe-spawn-socket orientation c active-handles remote-should-be-ground driver-fun)
  (match (list orientation c)
    [(or (list 'publisher (tcp-channel local-addr remote-addr _))
	 (list 'subscriber (tcp-channel remote-addr local-addr _)))
     (cond
      [(not (eqv? remote-should-be-ground (ground? remote-addr))) (transition active-handles)]
      [(not (ground? local-addr)) (transition active-handles)]
      [(set-member? active-handles (cons local-addr remote-addr)) (transition active-handles)]
      [else
       (transition (set-add active-handles (cons local-addr remote-addr))
	 (name-process (cons local-addr remote-addr)
	   (spawn (driver-fun local-addr remote-addr))))])]))

;; Orientation Topic Set<HandleMapping> -> Transition
(define (maybe-forget-socket orientation c active-handles)
  (match (list orientation c)
    [(or (list 'publisher (tcp-channel local-addr remote-addr _))
	 (list 'subscriber (tcp-channel remote-addr local-addr _)))
     (cond
      [(ground? remote-addr) (transition active-handles)]
      [(not (ground? local-addr)) (transition active-handles)]
      [else (transition (set-remove active-handles local-addr))])]))

;; TcpAddress TcpAddress -> Transition
(define (tcp-listener-manager local-addr dummy-remote-addr)
  (match-define (tcp-listener port) local-addr)
  (define listener (tcp:tcp-listen port 4 #t))

  (define (handle-absence orientation c state)
    ;; Hey, what if the presence we need went away between our manager
    ;; spawning us, and us getting to this point? Presence being
    ;; "edge-" rather than "level-triggered" means we'll hang around
    ;; sadly forever, accepting connections to nowhere. TODO
    (match (list orientation c)
      [(or (list 'publisher (tcp-channel (== local-addr) remote-addr _))
	   (list 'subscriber (tcp-channel remote-addr (== local-addr) _)))
       (if (ground? remote-addr)
	   (transition state)
	   (transition 'listener-is-closed
	     (quit)
	     (when (eq? state 'listener-is-running)
	       (name-process (list 'tcp-listener-closer local-addr)
		 (spawn (begin (tcp:tcp-close listener)
			       (transition 'dummy (quit))))))))]))

  (transition 'listener-is-running
    (observe-publishers/everything (tcp-channel local-addr any-remote (wild))
      (match-state state
	(match-conversation c
	  (on-absence (handle-absence 'publisher c state)))))
    (observe-subscribers/everything (tcp-channel any-remote local-addr (wild))
      (match-state state
	(match-conversation c
	  (on-absence (handle-absence 'subscriber c state)))))
    (subscriber (cons (tcp:tcp-accept-evt listener) (wild))
      (on-message
       [(cons _ (list cin cout))
	(let-values (((local-hostname local-port remote-hostname remote-port)
		      (tcp:tcp-addresses cin #t)))
	  (define remote-addr (tcp-address remote-hostname remote-port))
	  (name-process (cons local-addr remote-addr)
	    (spawn (tcp-connection-manager* local-addr remote-addr cin cout))))]))))

;; TcpAddress TcpAddress -> Transition
(define (tcp-connection-manager local-addr remote-addr)
  (match-define (tcp-address remote-hostname remote-port) remote-addr)
  (define-values (cin cout) (tcp:tcp-connect remote-hostname remote-port))
  (tcp-connection-manager* local-addr remote-addr cin cout))

(define (read-bytes-avail-evt len input-port)
  (guard-evt
   (lambda ()
     (let ([bstr (make-bytes len)])
       (wrap-evt
        (read-bytes-avail!-evt bstr input-port)
        (lambda (v)
          (if (number? v)
              (if (= v len) bstr (subbytes bstr 0 v))
              v)))))))

;; TcpAddress TcpAddress InputPort OutputPort -> Transition
;;
;; Our process state here is a Maybe<TcpConnectionState>, representing
;; a shutting-down state if #f.
(define (tcp-connection-manager* local-addr remote-addr cin cout)
  (define (close-transition is-open send-eof?)
    (transition #f
      (when is-open
	(list (when send-eof?
		(send-message (tcp-channel remote-addr local-addr eof)))
	      (name-process (list 'tcp-connection-closer local-addr remote-addr)
		(spawn (begin (tcp:tcp-abandon-port cin)
			      (tcp:tcp-abandon-port cout)
			      (transition/no-state (quit)))))))
      (quit)))

  (transition #t ;; open
    (subscriber (cons (read-bytes-avail-evt 4096 cin) (wild))
      (match-state is-open
	(on-message
	 [(cons _ (? eof-object?)) (close-transition is-open #t)]
	 [(cons _ (? bytes? bs)) (transition is-open
				   (send-message (tcp-channel remote-addr local-addr bs)))])))
    (subscriber (cons (eof-evt cin) (wild))
      (match-state is-open
	(on-message [(cons (? evt?) _) (close-transition is-open #t)])))
    (subscriber (tcp-channel local-addr remote-addr (wild))
      (match-state is-open
	(on-absence (close-transition is-open #f))
	(on-message
	 [(tcp-channel (== local-addr) (== remote-addr) subpacket)
	  (match subpacket
	    [(? eof-object?) (close-transition is-open #f)]
	    [(? string? s) (begin (write-string s cout)
				  (flush-output cout)
				  (transition is-open))]
	    [(? bytes? bs) (begin (write-bytes bs cout)
				  (flush-output cout)
				  (transition is-open))])])))
    (publisher (tcp-channel remote-addr local-addr (wild))
      (match-state is-open
	(on-absence (close-transition is-open #f))))))
