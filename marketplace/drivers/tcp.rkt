#lang racket/base
;; TCP drivers, ported from os2.rkt directly

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
	 (struct-out tcp-credit)
	 (struct-out tcp-mode)
	 send-tcp-credit
	 send-tcp-mode

	 tcp-driver
	 tcp-spy)

;; A TcpAddress is one of
;; -- a (tcp-address String Uint16), representing a remote socket
;; -- a (tcp-handle Any), representing a local socket on a kernel-assigned port
;; -- a (tcp-listener Uint16), representing a local socket on a user-assigned port
;; Note that tcp-handle-ids must be chosen carefully: they are scoped
;; to the local VM, i.e. shared between processes in that VM, so
;; processes must make sure not to accidentally clash in handle ID
;; selection. They are also used in TcpChannel to mean a specific
;; *instance* of a TCP connection, so if you are likely to want to
;; reconnect individual flows, use different tcp-handle-ids.
(struct tcp-address (host port) #:prefab)
(struct tcp-handle (id) #:prefab)
(struct tcp-listener (port) #:prefab)

;; A TcpChannel is a (tcp-channel TcpAddress TcpAddress TcpSubPacket),
;; and represents a section of a unidirectional TCP flow appearing on
;; our local "subnet" of the full TCP network, complete with source,
;; destination and subpacket.
(struct tcp-channel (source destination subpacket) #:prefab)

;; A TcpSubPacket is either
;; -- a Bytes, representing a section of the data carried by a
;;    TcpChannel. In principle, this should also have a sequence
;;    number field, but for simplicity we rely on os2.rkt's
;;    preservation of ordering.
;; -- an EndOfFile, representing the end of a the channel's stream.
;; -- a (tcp-credit NonNegativeInteger), for flow control.
;; -- a (tcp-mode TcpModeName), for mode selection.
(struct tcp-credit (amount) #:prefab)
(struct tcp-mode (name) #:prefab)

;; A TcpModeName is either
;; -- 'lines, for reading line-at-a-time, or
;; -- 'bytes, for reading chunks of bytes.

;; TODO: BUG?: Routing packets between two local sockets won't work
;; because the patterns aren't set up to recognise that situation.

;; A TcpConnectionState is a (tcp-connection-state TcpModeName
;; Integer), representing the current input mode and issued credit.
(struct tcp-connection-state (mode credit) #:prefab)

;; TcpAddress TcpAddress NonNegativeInteger -> Preaction
;; Sends a credit message on a channel using the correct (subscriber) role.
(define (send-tcp-credit source-addr sink-addr amount)
  (send-feedback (tcp-channel source-addr sink-addr (tcp-credit amount))))

;; TcpAddress TcpAddress TcpModeName -> Preaction
;; Sends a mode selection message on a channel using the correct (subscriber) role.
(define (send-tcp-mode source-addr sink-addr mode-name)
  (send-feedback (tcp-channel source-addr sink-addr (tcp-mode mode-name))))

;; TcpAddresses; represents various wildcard addresses
(define any-remote (tcp-address (wild) (wild)))
(define any-handle (tcp-handle (wild)))
(define any-listener (tcp-listener (wild)))

;; Spawn
;; Process acting as a TCP socket factory.
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

;; Role Set<HandleMapping> Boolean (TcpAddress TcpAddress -> BootK) -> Transition
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

;; TcpAddress TcpAddress InputPort OutputPort -> Transition
;;
;; Our process state here is a Maybe<TcpConnectionState>, representing
;; a shutting-down state if #f.
(define (tcp-connection-manager* local-addr remote-addr cin cout)
  (define (close-transition state send-eof?)
    (transition #f
      (when (not (eq? state #f))
	(list (when send-eof?
		(send-message (tcp-channel remote-addr local-addr eof)))
	      (spawn #:debug-name (list 'tcp-connection-closer local-addr remote-addr)
		     #:child
		     (begin (tcp:tcp-abandon-port cin)
			    (tcp:tcp-abandon-port cout)
			    (transition 'dummy (quit))))))
      (quit)))
  (define (adjust-credit state amount)
    (let ((new-credit (+ (tcp-connection-state-credit state) amount)))
      (transition (struct-copy tcp-connection-state state [credit new-credit])
	(delete-endpoint 'inbound-relay)
	(when (positive? new-credit)
	  (case (tcp-connection-state-mode state)
	    [(lines)
	     (endpoint #:subscriber (cons (read-bytes-line-evt cin 'any) (wild))
		       #:name 'inbound-relay
		       #:state state
	      [(cons _ (? eof-object?))
	       (close-transition state #t)]
	      [(cons _ (? bytes? bs))
	       (sequence-actions (adjust-credit state -1)
				 (send-message (tcp-channel remote-addr local-addr bs)))])]
	    [(bytes)
	     (endpoint #:subscriber (cons (read-bytes-evt new-credit cin) (wild))
		       #:name 'inbound-relay
		       #:state state
	      [(cons _ (? eof-object?))
	       (close-transition state #t)]
	      [(cons _ (? bytes? bs))
	       (let ((len (bytes-length bs)))
		 (sequence-actions (adjust-credit state (- len))
				   (send-message (tcp-channel remote-addr local-addr bs))))])])))))
  (transition (tcp-connection-state 'bytes 0)
    (endpoint #:subscriber (cons (eof-evt cin) (wild))
	      #:state state
     [(cons (? evt?) _)
      (close-transition state #t)])
    (endpoint #:subscriber (tcp-channel local-addr remote-addr (wild))
	      #:state state
	      #:on-absence (close-transition state #f)
     [(tcp-channel (== local-addr) (== remote-addr) subpacket)
      (match subpacket
	[(? eof-object?) (close-transition state #f)]
	[(? bytes? bs)
	 (define len (bytes-length bs))
	 (write-bytes bs cout)
	 (flush-output cout)
	 (transition state (send-tcp-credit local-addr remote-addr len))]
	[_
	 (error 'tcp-connection-manager*
		"Publisher on a channel isn't supposed to issue channel control messages")])])
    (endpoint #:publisher (tcp-channel remote-addr local-addr (wild))
	      #:state state
	      #:on-absence (close-transition state #f)
     [(tcp-channel (== remote-addr) (== local-addr) subpacket)
      (match subpacket
	[(tcp-credit amount)
	 (if state (adjust-credit state amount) (transition state))]
	[(tcp-mode new-mode)
	 ;; Also resets credit to zero.
	 (if state (adjust-credit (tcp-connection-state new-mode 0) 0) (transition state))]
	[_
	 (error 'tcp-connection-manager*
		"Subscriber on a channel may only send channel control messages")])])))

;; Spawn
;; Debugging aid: produces pretty hex dumps of TCP traffic sent on
;; this network. Also prints out other messages without special
;; formatting.
(define (tcp-spy)

  (define (display-message m)
    (match m
      [(tcp-channel source dest (? bytes? body))
       (write `(TCPDATA ,source --> ,dest)) (newline)
       (dump-bytes! body (bytes-length body))
       (void)]
      [(tcp-channel source dest (? eof-object?))
       (write `(TCPEOF ,source --> ,dest)) (newline)
       (void)]
      [(tcp-channel source dest (tcp-credit amount))
       (write `(TCPCREDIT ,source --> ,dest ,amount)) (newline)
       (void)]
      [other
       (write `(TCPOTHER ,other)) (newline)
       (void)]))

  (spawn #:debug-name 'tcp-spy
	 #:child
	 (transition 'no-state
	   (endpoint #:subscriber (wild) #:observer [m (display-message m)])
	   (endpoint #:publisher (wild) #:observer [m (display-message m)]))))
