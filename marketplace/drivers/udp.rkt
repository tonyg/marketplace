#lang typed/racket/base
;; UDP driver.

(require racket/set)
(require racket/match)
(require racket/udp)
(require "../sugar-typed.rkt")
(require "../support/event.rkt")
(require "../support/pseudo-substruct.rkt")

(provide (struct-out udp-remote-address-repr)
	 UdpRemoteAddress udp-remote-address udp-remote-address?
	 UdpRemoteAddressPattern udp-remote-address-pattern udp-remote-address-pattern?

	 (struct-out udp-handle-repr)
	 UdpHandle udp-handle udp-handle?
	 UdpHandlePattern udp-handle-pattern udp-handle-pattern?

	 (struct-out udp-listener-repr)
	 UdpListener udp-listener udp-listener?
	 UdpListenerPattern udp-listener-pattern udp-listener-pattern?

	 UdpAddress
	 UdpAddressPattern

	 UdpLocalAddress

	 udp-address?
	 udp-address-pattern?
	 udp-local-address?

	 (struct-out udp-packet-repr)
	 UdpPacket udp-packet udp-packet?
	 UdpPacketPattern udp-packet-pattern udp-packet-pattern?

	 udp-driver)

;; A UdpAddress is one of
;; -- a (udp-address String Uint16), representing a remote socket
;; -- a (udp-handle Any), representing a local socket on a kernel-assigned port
;; -- a (udp-listener Uint16), representing a local socket on a user-assigned port
;; Note that udp-handle-ids must be chosen carefully: they are scoped
;; to the local VM, i.e. shared between processes in that VM, so
;; processes must make sure not to accidentally clash in handle ID
;; selection.
(struct: (THost TPort)
	 udp-remote-address-repr ([host : THost]
				  [port : TPort])
	 #:prefab)
(pseudo-substruct: (udp-remote-address-repr String Natural)
		   UdpRemoteAddress udp-remote-address udp-remote-address?)
(pseudo-substruct: (udp-remote-address-repr (U Wild String) (U Wild Natural))
		   UdpRemoteAddressPattern udp-remote-address-pattern udp-remote-address-pattern?)

(struct: (TId)
	 udp-handle-repr ([id : TId])
	 #:prefab)
(pseudo-substruct: (udp-handle-repr Any)
		   UdpHandle udp-handle udp-handle?)
(pseudo-substruct: (udp-handle-repr (U Wild Any))
		   UdpHandlePattern udp-handle-pattern udp-handle-pattern?)

(struct: (TPort)
	 udp-listener-repr ([port : TPort])
	 #:prefab)
(pseudo-substruct: (udp-listener-repr Natural)
		   UdpListener udp-listener udp-listener?)
(pseudo-substruct: (udp-listener-repr (U Wild Natural))
		   UdpListenerPattern udp-listener-pattern udp-listener-pattern?)

(define-type UdpAddress (U UdpRemoteAddress UdpHandle UdpListener))
(define-type UdpAddressPattern (U Wild UdpRemoteAddressPattern UdpHandlePattern UdpListenerPattern))

(define-type UdpLocalAddress (U UdpHandle UdpListener))

(define-predicate udp-address? UdpAddress)
(define-predicate udp-address-pattern? UdpAddressPattern)
(define-predicate udp-local-address? UdpLocalAddress)

;; A UdpPacket is a (udp-packet UdpAddress UdpAddress Bytes), and
;; represents a packet appearing on our local "subnet" of the full UDP
;; network, complete with source, destination and contents.
(struct: (TSource TDestination TBody)
	 udp-packet-repr ([source : TSource]
			  [destination : TDestination]
			  [body : TBody])
	 #:prefab)
(pseudo-substruct: (udp-packet-repr UdpAddress UdpAddress Bytes)
		   UdpPacket udp-packet udp-packet?)
(pseudo-substruct: (udp-packet-repr UdpAddressPattern UdpAddressPattern (U Wild Bytes))
		   UdpPacketPattern udp-packet-pattern udp-packet-pattern?)

;; A HandleMapping is a record describing a mapping between a local
;; UdpAddress and its underlying UDP socket. It's private to the
;; implementation of the driver.
(struct: (TAddress TSocket)
	 handle-mapping-repr ([address : TAddress]
			      [socket : TSocket])
	 #:prefab)
(pseudo-substruct: (handle-mapping-repr UdpLocalAddress Any)
		   ;; ^ TODO: Want to use UDP-Socket instead of Any here
		   HandleMapping handle-mapping handle-mapping?)
(pseudo-substruct: (handle-mapping-repr (U Wild UdpLocalAddress) (U Wild Any))
		   HandleMappingPattern handle-mapping-pattern handle-mapping-pattern?)

;; TODO: BUG?: Routing packets between two local sockets won't work
;; because the patterns aren't set up to recognise that situation.

;; represents any remote address
(: any-remote : UdpAddressPattern)
(define any-remote (udp-remote-address-pattern (wild) (wild)))

(define-type DriverState (Setof UdpLocalAddress))

(define-type SocketManagerState Boolean)

;; Process acting as a UDP socket factory.
(: udp-driver : (All (ParentState) -> (Spawn ParentState)))
(define (udp-driver)

  (: handle-presence : Topic DriverState -> (Transition DriverState))
  (define (handle-presence topic active-handles)
    (match-define (udp-packet-pattern _ (? udp-local-address? local-addr) _) topic)
    (cond
     [(set-member? active-handles local-addr)
      (transition: active-handles : DriverState)]
     [else
      (transition: (set-add active-handles local-addr) : DriverState
	(udp-socket-manager local-addr))]))

  (spawn: #:debug-name 'udp-driver
	  #:parent : ParentState
	  #:child : DriverState
	  (transition: ((inst set UdpLocalAddress)) : DriverState
	    (endpoint: active-handles : DriverState
		       #:publisher
		       (udp-packet-pattern any-remote (udp-handle-pattern (wild)) (wild))
		       #:observer
		       #:conversation topic
		       #:on-presence (handle-presence topic active-handles))
	    (endpoint: active-handles : DriverState
		       #:publisher
		       (udp-packet-pattern any-remote (udp-listener-pattern (wild)) (wild))
		       #:observer
		       #:conversation topic
		       #:on-presence (handle-presence topic active-handles))
	    (endpoint: active-handles : DriverState
		       #:subscriber
		       (udp-packet-pattern any-remote (udp-handle-pattern (wild)) (wild))
		       #:observer
		       #:conversation topic
		       #:on-presence (handle-presence topic active-handles))
	    (endpoint: active-handles : DriverState
		       #:subscriber
		       (udp-packet-pattern any-remote (udp-listener-pattern (wild)) (wild))
		       #:observer
		       #:conversation topic
		       #:on-presence (handle-presence topic active-handles))
	    (endpoint: active-handles : DriverState
		       #:subscriber (handle-mapping-pattern (wild) (wild))
		       #:observer
		       #:conversation (handle-mapping local-addr socket)
		       #:on-absence
		       (transition: (set-remove active-handles local-addr) : DriverState))
	    )))

(: bind-socket! : UDP-Socket UdpLocalAddress -> Void)
(define (bind-socket! s local-addr)
  (match local-addr
    [(udp-listener port) (udp-bind! s #f port)]
    [(udp-handle _) (udp-bind! s #f 0)]
    [else (void)]))

(: valid-port-number? : Any -> Boolean : Natural)
(define (valid-port-number? x)
  ;; Eventually TR will know about ranges
  (exact-nonnegative-integer? x))

(: udp-socket-manager : UdpLocalAddress -> (Spawn DriverState))
(define (udp-socket-manager local-addr)
  (define s (udp-open-socket #f #f))
  (bind-socket! s local-addr)
  (define buffer (make-bytes 65536)) ;; TODO: buffer size control

  (: handle-absence : SocketManagerState -> (Transition SocketManagerState))
  (define (handle-absence socket-is-open?)
    (transition: #f : SocketManagerState
      (quit)
      (when socket-is-open?
	(spawn: #:debug-name `(udp-socket-closer ,local-addr)
		#:parent : SocketManagerState
		#:child : Void
		(begin (udp-close s)
		       (transition: (void) : Void (quit)))))))

  (spawn: #:debug-name `(udp-socket-manager ,local-addr)
	  #:parent : DriverState
	  #:child : SocketManagerState
	  (transition: #t : SocketManagerState
	    ;; Offers a handle-mapping on the local network so that
	    ;; the driver/factory can clean up when this process dies.
	    (endpoint: : SocketManagerState #:publisher (handle-mapping local-addr s))
	    ;; If our counterparty removes either of their endpoints
	    ;; as the subscriber end of the remote-to-local stream or
	    ;; the publisher end of the local-to-remote stream, shut
	    ;; ourselves down. Also, relay messages published on the
	    ;; local-to-remote stream out on the actual socket.
	    (endpoint: socket-is-open? : SocketManagerState
		       #:publisher (udp-packet-pattern any-remote local-addr (wild))
		       #:on-absence (handle-absence socket-is-open?))
	    (endpoint: socket-is-open? : SocketManagerState
		       #:subscriber (udp-packet-pattern local-addr any-remote (wild))
		       #:on-absence (handle-absence socket-is-open?)
		       [(udp-packet (== local-addr)
				    (udp-remote-address remote-host remote-port)
				    body)
			(begin (udp-send-to s remote-host remote-port body)
			       (transition: socket-is-open? : SocketManagerState))])
	    ;; Listen for messages arriving on the actual socket using
	    ;; a ground event, and relay them at this level.
	    (endpoint: : SocketManagerState
		       #:subscriber (cons (udp-receive!-evt s buffer) (wild))
		       [(cons (? evt?) (list (? exact-integer? packet-length)
					     (? string? remote-host)
					     (? valid-port-number? remote-port)))
			(let ((packet (subbytes buffer 0 packet-length)))
			  (send-message (udp-packet (udp-remote-address remote-host remote-port)
						    local-addr
						    packet)))]))))
