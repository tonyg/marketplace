#lang racket/base
;; TCP driver, with flow-control and line discipline removed, sans reliance on (ground?)

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
	 (match-interest-type 'observer
	   (match-state active-handles
	     (match-conversation (tcp-channel L _ _)
	       (on-presence (maybe-spawn-socket 'incoming L active-handles tcp-listener-manager))
	       (on-absence (maybe-forget-socket 'incoming L active-handles))))))
       (observe-subscribers/everything (tcp-channel any-remote any-listener (wild))
	 (match-interest-type 'observer
	   (match-state active-handles
	     (match-conversation (tcp-channel _ L _)
	       (on-presence (maybe-spawn-socket 'incoming L active-handles tcp-listener-manager))
	       (on-absence (maybe-forget-socket 'incoming L active-handles))))))
       (observe-publishers (tcp-channel any-handle any-remote (wild))
	 (match-state active-handles
	   (match-conversation (tcp-channel L R _)
	     (on-presence (maybe-spawn-socket R L active-handles tcp-connection-manager))
	     (on-absence (maybe-forget-socket R L active-handles)))))
       (observe-subscribers (tcp-channel any-remote any-handle (wild))
	 (match-state active-handles
	   (match-conversation (tcp-channel R L _)
	     (on-presence (maybe-spawn-socket R L active-handles tcp-connection-manager))
	     (on-absence (maybe-forget-socket R L active-handles)))))))))

(define tcp (tcp-driver)) ;; pre-instantiated!

(define (maybe-spawn-socket R L active-handles driver-fun)
  (define name (cons L R))
  (if (set-member? active-handles name)
      (transition active-handles)
      (transition (set-add active-handles name)
	(name-process name (spawn (driver-fun L R))))))

(define (maybe-forget-socket R L active-handles)
  (define name (cons L R))
  (transition (set-remove active-handles name)))

;; TcpAddress 'incoming -> Transition
(define (tcp-listener-manager local-addr dummy-incoming-marker)
  (match-define (tcp-listener port) local-addr)
  (define listener (tcp:tcp-listen port 4 #t))

  (define (handle-absence)
    ;; Hey, what if the presence we need went away between our manager
    ;; spawning us, and us getting to this point? Presence being
    ;; "edge-" rather than "level-triggered" means we'll hang around
    ;; sadly forever, accepting connections to nowhere. TODO
    (transition 'listener-is-closed
      (name-process (list 'tcp-listener-closer local-addr)
	(spawn (begin (tcp:tcp-close listener)
		      (transition 'dummy (quit)))))
      (quit)))

  (transition 'listener-is-running
    (observe-publishers/everything (tcp-channel local-addr any-remote (wild))
      (match-interest-type 'observer
	(match-state 'listener-is-running
	  (on-absence (handle-absence)))))
    (observe-subscribers/everything (tcp-channel any-remote local-addr (wild))
      (match-interest-type 'observer
	(match-state 'listener-is-running
	  (on-absence (handle-absence)))))
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
;; Our process state here is either 'open or 'closing.
(define (tcp-connection-manager* local-addr remote-addr cin cout)
  (define (close-transition send-eof?)
    (transition 'closing
      (when send-eof? (send-message (tcp-channel remote-addr local-addr eof)))
      (name-process (list 'tcp-connection-closer local-addr remote-addr)
	(spawn (begin (tcp:tcp-abandon-port cin)
		      (tcp:tcp-abandon-port cout)
		      (transition/no-state (quit)))))
      (quit)))

  (transition 'open
    (subscriber (cons (read-bytes-avail-evt 4096 cin) (wild))
      (match-state 'open
	(on-message
	 [(cons _ (? eof-object?)) (close-transition #t)]
	 [(cons _ (? bytes? bs)) (transition 'open
				   (send-message (tcp-channel remote-addr local-addr bs)))])))
    (subscriber (cons (eof-evt cin) (wild))
      (match-state 'open
	(on-message [(cons (? evt?) _) (close-transition #t)])))
    (subscriber (tcp-channel local-addr remote-addr (wild))
      (match-state 'open
	(on-absence (close-transition #f))
	(on-message
	 [(tcp-channel (== local-addr) (== remote-addr) subpacket)
	  (match subpacket
	    [(? eof-object?) (close-transition #f)]
	    [(? string? s) (begin (write-string s cout)
				  (flush-output cout)
				  (transition 'open))]
	    [(? bytes? bs) (begin (write-bytes bs cout)
				  (flush-output cout)
				  (transition 'open))])])))
    (publisher (tcp-channel remote-addr local-addr (wild))
      (match-state 'open
	(on-absence (close-transition #f))))))
