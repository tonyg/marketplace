#lang racket/base
;; UDP driver.

(require racket/set)
(require racket/match)
(require racket/udp)

(require "../sugar.rkt")

(provide (struct-out udp-remote-address)
	 (struct-out udp-handle)
	 (struct-out udp-listener)

	 udp-address?
	 udp-local-address?

	 (struct-out udp-packet)
	 udp-driver)

;; A UdpAddress is one of
;; -- a (udp-address String Uint16), representing a remote socket
;; -- a (udp-handle Any), representing a local socket on a kernel-assigned port
;; -- a (udp-listener Uint16), representing a local socket on a user-assigned port
;; Note that udp-handle-ids must be chosen carefully: they are scoped
;; to the local VM, i.e. shared between processes in that VM, so
;; processes must make sure not to accidentally clash in handle ID
;; selection.
(struct udp-remote-address (host port) #:transparent)
(struct udp-handle (id) #:transparent)
(struct udp-listener (port) #:transparent)

(define (udp-address? x)
  (or (udp-remote-address? x)
      (udp-handle? x)
      (udp-listener? x)))

(define (udp-local-address? x)
  (or (udp-handle? x)
      (udp-listener? x)))

;; A UdpPacket is a (udp-packet UdpAddress UdpAddress Bytes), and
;; represents a packet appearing on our local "subnet" of the full UDP
;; network, complete with source, destination and contents.
(struct udp-packet (source destination body) #:transparent)

;; A HandleMapping is a record describing a mapping between a local
;; UdpAddress and its underlying UDP socket. It's private to the
;; implementation of the driver.
(struct handle-mapping (address socket) #:transparent)

;; TODO: BUG?: Routing packets between two local sockets won't work
;; because the patterns aren't set up to recognise that situation.

;; represents any remote address
;; any-remote : UdpAddressPattern
(define any-remote (udp-remote-address (wild) (wild)))

;; (define-type DriverState (Setof UdpLocalAddress))

;; (define-type SocketManagerState Boolean)

;; Process acting as a UDP socket factory.
;; udp-driver : (All (ParentState) -> (Spawn ParentState))
(define (udp-driver)

  ;; handle-presence : Topic DriverState -> (Transition DriverState)
  (define (handle-presence topic active-handles)
    (match-define (udp-packet _ (? udp-local-address? local-addr) _) topic)
    (cond
     [(set-member? active-handles local-addr)
      (transition active-handles)]
     [else
      (transition (set-add active-handles local-addr)
	(udp-socket-manager local-addr))]))

  (name-process 'udp-driver
    (spawn (transition (set)

	     (observe-subscribers (udp-packet any-remote (udp-handle (wild)) (wild))
	       (match-state active-handles
		 (match-conversation topic
		   (on-presence (handle-presence topic active-handles)))))
	     (observe-subscribers (udp-packet any-remote (udp-listener (wild)) (wild))
	       (match-state active-handles
		 (match-conversation topic
		   (on-presence (handle-presence topic active-handles)))))
	     (observe-publishers (udp-packet any-remote (udp-handle (wild)) (wild))
	       (match-state active-handles
		 (match-conversation topic
		   (on-presence (handle-presence topic active-handles)))))
	     (observe-publishers (udp-packet any-remote (udp-listener (wild)) (wild))
	       (match-state active-handles
		 (match-conversation topic
		   (on-presence (handle-presence topic active-handles)))))

	     (observe-publishers (handle-mapping (wild) (wild))
	       (match-state active-handles
		 (match-conversation (handle-mapping local-addr socket)
		   (on-absence
		    (transition (set-remove active-handles local-addr))))))
	     ))))

;; bind-socket! : UDP-Socket UdpLocalAddress -> Void
(define (bind-socket! s local-addr)
  (match local-addr
    [(udp-listener port) (udp-bind! s #f port)]
    [(udp-handle _) (udp-bind! s #f 0)]
    [else (void)]))

;; valid-port-number? : Any -> Boolean : Natural
(define (valid-port-number? x)
  ;; Eventually TR will know about ranges
  (exact-nonnegative-integer? x))

;; udp-socket-manager : UdpLocalAddress -> (Spawn DriverState)
(define (udp-socket-manager local-addr)
  (define s (udp-open-socket #f #f))
  (bind-socket! s local-addr)
  (define buffer (make-bytes 65536)) ;; TODO: buffer size control

  ;; handle-absence : SocketManagerState -> (Transition SocketManagerState)
  (define (handle-absence socket-is-open?)
    (transition #f
      (quit)
      (when socket-is-open?
	(name-process `(udp-socket-closer ,local-addr)
	  (spawn (begin (udp-close s)
			(transition (void) (quit))))))))

  (name-process `(udp-socket-manager ,local-addr)
    (spawn (transition #t
	      ;; Offers a handle-mapping on the local network so that
	      ;; the driver/factory can clean up when this process dies.
	      (publisher (handle-mapping local-addr s))
	      ;; If our counterparty removes either of their endpoints
	      ;; as the subscriber end of the remote-to-local stream or
	      ;; the publisher end of the local-to-remote stream, shut
	      ;; ourselves down. Also, relay messages published on the
	      ;; local-to-remote stream out on the actual socket.
	      (publisher (udp-packet any-remote local-addr (wild))
		(match-state socket-is-open?
		  (on-absence (handle-absence socket-is-open?))))
	      (subscriber (udp-packet local-addr any-remote (wild))
		(match-state socket-is-open?
		  (on-absence (handle-absence socket-is-open?))
		  (on-message
		   [(udp-packet (== local-addr)
				(udp-remote-address remote-host remote-port)
				body)
		    (begin (udp-send-to s remote-host remote-port body)
			   (transition socket-is-open?))])))
	      ;; Listen for messages arriving on the actual socket using
	      ;; a ground event, and relay them at this level.
	      (subscriber (cons (udp-receive!-evt s buffer) (wild))
		(on-message
		 [(cons (? evt?) (list (? exact-integer? packet-length)
				       (? string? remote-host)
				       (? valid-port-number? remote-port)))
		  (let ((packet (subbytes buffer 0 packet-length)))
		    (send-message (udp-packet (udp-remote-address remote-host remote-port)
					      local-addr
					      packet)))]))))))
