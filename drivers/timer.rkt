#lang racket/base
;; Timer driver.

;; Uses mutable state internally, but because the scope of the
;; mutation is limited to each timer process alone, it's easy to show
;; correct linear use of the various pointers.

(require racket/set)
(require racket/match)
(require data/heap)
(require "../sugar.rkt")

;; (pending-timer AbsoluteSeconds Any Boolean)
;; An outstanding timer being managed by the timer-driver.
(struct pending-timer (deadline ;; Real
		       label ;; TimerLabel
		       )
	#:transparent)

(provide (struct-out set-timer)
	 (struct-out timer-expired)
	 timer-driver
	 timer-relay)

;; (define-type TimerKind (U 'relative 'absolute))

;; The timer driver and timer relays listen for messages of this type,
;; and when they hear one, they set an alarm that will later send a
;; corresponding timer-expired message.
(struct set-timer (label msecs kind) #:transparent)

;; Message sent by the timer driver or a timer relay upon expiry of a
;; timer. Contains the label specified in the corresponding set-timer
;; message, and also the current absolute time from the outside world.
(struct timer-expired (label msecs) #:transparent)

;; State of a timer-driver, including the identifier of the driver,
;; the currently-active subscription to ground time events (if any),
;; and the heap of all remaining timers.
(struct driver-state (heap) #:transparent)

;; (define-type RelayKey Exact-Nonnegative-Integer)

;; State of a timer-relay, including the next timer number and a
;; mapping from timer number to timer label.
(struct relay-state (next-counter ;; RelayKey
		     active-timers ;; (HashTable RelayKey TimerLabel)
		     )
	#:transparent)

;; (define-type RelayState relay-state)

;; Note that (set-timer 'current-time 0 #f) causes an immediate reply
;; of (timer-expired 'current-time (current-inexact-milliseconds)),
;; which can be used for an event-oriented interface to reading the
;; system clock.

;; Racket's alarm-evt is almost the right design for timeouts: its
;; synchronisation value should be the (or some) value of the clock
;; after the asked-for time. That way it serves as timeout and
;; clock-reader in one.
;; timer-evt : Real -> Evt
(define (timer-evt msecs)
  (wrap-evt (alarm-evt msecs)
	    (lambda (_) (current-inexact-milliseconds))))

;; make-timer-heap : -> Heap
(define (make-timer-heap)
  (make-heap (lambda (t1 t2) (<= (pending-timer-deadline t1) (pending-timer-deadline t2)))))

;; Retrieves the earliest-deadline timer from the heap, if there is
;; one.
;; next-timer! : Heap -> (Option pending-timer)
(define (next-timer! heap)
  (if (zero? (heap-count heap))
      #f
      (heap-min heap)))

;; Retrieves (and removes) all timers from the heap that have deadline
;; earlier or equal to the time passed in.
;; fire-timers! : Heap Real -> (Listof SendMessage)
(define (fire-timers! heap now)
  (if (zero? (heap-count heap))
      '()
      (let ((m (heap-min heap)))
	(if (<= (pending-timer-deadline m) now)
	    (begin (heap-remove-min! heap)
		   (cons (send-message (timer-expired (pending-timer-label m) now))
			 (fire-timers! heap now)))
	    '()))))

;; Process for mapping this-level timer requests to ground-level timer
;; events and back.
;; timer-driver : (All (ParentState) -> (Spawn ParentState))
(define (timer-driver)
  (name-process 'timer-driver
    (spawn (transition (driver-state (make-timer-heap))
	     (subscriber (set-timer (wild) (wild) (wild))
	       (match-state state
		 (on-message
		  [(set-timer label msecs 'relative)
		   (install-timer! state label (+ (current-inexact-milliseconds) msecs))]
		  [(set-timer label msecs 'absolute)
		   (install-timer! state label msecs)])))
	     (publisher (timer-expired (wild) (wild)))))))

;; install-timer! : DriverState TimerLabel Real -> (Transition DriverState)
(define (install-timer! state label deadline)
  (heap-add! (driver-state-heap state) (pending-timer deadline label))
  (update-time-listener! state))

;; update-time-listener! : DriverState -> (Transition DriverState)
(define (update-time-listener! state)
  (define next (next-timer! (driver-state-heap state)))
  (transition state
    (delete-endpoint 'time-listener)
    (and next
	 (name-endpoint 'time-listener
	   (subscriber (cons (timer-evt (pending-timer-deadline next)) (wild))
	     (match-state state
	       (on-message
		[(cons (? evt?) (? real? now))
		 (let ((to-send (fire-timers! (driver-state-heap state) now)))
		   ;; Note: compute to-send before recursing, because of side-effects on heap
		   (sequence-actions (transition state)
		     update-time-listener!
		     to-send))])))))))

;; Process for mapping this-level timer requests to meta-level timer
;; requests. Useful when running nested VMs: essentially extends timer
;; support up the branches of the VM tree toward the leaves.
;; timer-relay : (All (ParentState) Symbol -> (Spawn ParentState))
(define (timer-relay self-id)
  (name-process `(timer-relay ,self-id)
    (spawn (transition (relay-state 0 (make-immutable-hash '()))
	     (at-meta-level
	      (subscriber (timer-expired (wild) (wild))
		(match-state (relay-state next-counter active-timers)
		  (on-message
		   [(timer-expired (list (== self-id) (? exact-nonnegative-integer? counter))
				   now)
		    (transition (relay-state next-counter (hash-remove active-timers counter))
		      (and (hash-has-key? active-timers counter)
			   (send-message (timer-expired (hash-ref active-timers counter)
							now))))]))))
	     (subscriber (set-timer (wild) (wild) (wild))
	       (match-state (relay-state next-counter active-timers)
		 (on-message
		  [(set-timer label msecs kind)
		   (transition (relay-state (+ next-counter 1)
					    (hash-set active-timers next-counter label))
		     (at-meta-level
		      (send-message (set-timer (list self-id next-counter) msecs kind))))])))
	     (publisher (timer-expired (wild) (wild)))))))
