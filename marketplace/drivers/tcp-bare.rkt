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
  (spawn #:debug-name 'tcp-driver
	 #:child
   (transition (set)
     (endpoint #:subscriber (tcp-channel any-listener any-remote (wild)) #:everything
	       #:state active-handles
	       #:role r
	       #:on-presence (maybe-spawn-socket r active-handles #f tcp-listener-manager)
	       #:on-absence (maybe-forget-socket r active-handles))
     (endpoint #:publisher  (tcp-channel any-remote any-listener (wild)) #:everything
	       #:state active-handles
	       #:role r
	       #:on-presence (maybe-spawn-socket r active-handles #f tcp-listener-manager)
	       #:on-absence (maybe-forget-socket r active-handles))
     (endpoint #:subscriber (tcp-channel any-handle any-remote (wild)) #:observer
	       #:state active-handles
	       #:role r
	       #:on-presence (maybe-spawn-socket r active-handles #t tcp-connection-manager)
	       #:on-absence (maybe-forget-socket r active-handles))
     (endpoint #:publisher  (tcp-channel any-remote any-handle (wild)) #:observer
	       #:state active-handles
	       #:role r
	       #:on-presence (maybe-spawn-socket r active-handles #t tcp-connection-manager)
	       #:on-absence (maybe-forget-socket r active-handles)))))

(define tcp (tcp-driver)) ;; pre-instantiated!

(define (maybe-spawn-socket r active-handles remote-should-be-ground driver-fun)
  (match r
    [(or (role 'publisher (tcp-channel local-addr remote-addr _) _)
	 (role 'subscriber (tcp-channel remote-addr local-addr _) _))
     (cond
      [(not (eqv? remote-should-be-ground (ground? remote-addr))) (transition active-handles)]
      [(not (ground? local-addr)) (transition active-handles)]
      [(set-member? active-handles (cons local-addr remote-addr)) (transition active-handles)]
      [else
       (transition (set-add active-handles (cons local-addr remote-addr))
	 (spawn #:debug-name (cons local-addr remote-addr)
		#:child (driver-fun local-addr remote-addr)))])]))

;; Role Set<HandleMapping> -> Transition
(define (maybe-forget-socket r active-handles)
  (match r
    [(or (role 'publisher (tcp-channel local-addr remote-addr _) _)
	 (role 'subscriber (tcp-channel remote-addr local-addr _) _))
     (cond
      [(ground? remote-addr) (transition active-handles)]
      [(not (ground? local-addr)) (transition active-handles)]
      [else (transition (set-remove active-handles local-addr))])]))

;; TcpAddress TcpAddress -> Transition
(define (tcp-listener-manager local-addr dummy-remote-addr)
  (match-define (tcp-listener port) local-addr)
  (define listener (tcp:tcp-listen port 4 #t))

  (define (handle-absence r state)
    ;; Hey, what if the presence we need went away between our manager
    ;; spawning us, and us getting to this point? Presence being
    ;; "edge-" rather than "level-triggered" means we'll hang around
    ;; sadly forever, accepting connections to nowhere. TODO
    (match r
      [(or (role 'publisher (tcp-channel (== local-addr) remote-addr _) _)
	   (role 'subscriber (tcp-channel remote-addr (== local-addr) _) _))
       (if (ground? remote-addr)
	   (transition state)
	   (transition 'listener-is-closed
	     (quit)
	     (when (eq? state 'listener-is-running)
	       (spawn #:debug-name (list 'tcp-listener-closer local-addr)
		      #:child
		      (begin (tcp:tcp-close listener)
			     (transition 'dummy (quit)))))))]))

  (transition 'listener-is-running
    (endpoint #:subscriber (tcp-channel local-addr any-remote (wild)) #:everything
	      #:state state
	      #:role r
	      #:on-absence (handle-absence r state))
    (endpoint #:publisher  (tcp-channel any-remote local-addr (wild)) #:everything
	      #:state state
	      #:role r
	      #:on-absence (handle-absence r state))
    (endpoint #:subscriber (cons (tcp:tcp-accept-evt listener) (wild))
	      [(cons _ (list cin cout))
	       (let-values (((local-hostname local-port remote-hostname remote-port)
			     (tcp:tcp-addresses cin #t)))
		 (define remote-addr (tcp-address remote-hostname remote-port))
		 (spawn #:debug-name (cons local-addr remote-addr)
			#:child (tcp-connection-manager* local-addr remote-addr cin cout)))])))

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
	      (spawn #:debug-name (list 'tcp-connection-closer local-addr remote-addr)
		     #:child
		     (begin (tcp:tcp-abandon-port cin)
			    (tcp:tcp-abandon-port cout)
			    (transition/no-state (quit))))))
      (quit)))

  (transition #t ;; open
    (endpoint #:subscriber (cons (read-bytes-avail-evt 4096 cin) (wild))
	      #:state is-open
	      [(cons _ (? eof-object?)) (close-transition is-open #t)]
	      [(cons _ (? bytes? bs)) (transition is-open (send-message (tcp-channel remote-addr local-addr bs)))])
    (endpoint #:subscriber (cons (eof-evt cin) (wild))
	      #:state is-open
	      [(cons (? evt?) _) (close-transition is-open #t)])
    (endpoint #:subscriber (tcp-channel local-addr remote-addr (wild))
	      #:state is-open
	      #:on-absence (close-transition is-open #f)
	      [(tcp-channel (== local-addr) (== remote-addr) subpacket)
	       (match subpacket
		 [(? eof-object?) (close-transition is-open #f)]
		 [(? string? s) (begin (write-string s cout)
				       (flush-output cout)
				       (transition is-open))]
		 [(? bytes? bs) (begin (write-bytes bs cout)
				       (flush-output cout)
				       (transition is-open))])])
    (endpoint #:publisher (tcp-channel remote-addr local-addr (wild))
	      #:state is-open
	      #:on-absence (close-transition is-open #f))))
