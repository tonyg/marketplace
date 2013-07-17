#lang typed/racket/base

(require "quasiqueue.rkt")

(require/typed "opaque-any.rkt"
	       ;; Various opaque "Any"s
	       [opaque Topic topic?]
	       [opaque PreEID pre-eid?]
	       [opaque Reason reason?])

(provide (all-defined-out)
	 (all-from-out "quasiqueue.rkt"))

;; This module uses different terminology to os2.rkt. From the paper:
;; "A role generalizes traditional notions of advertisement and
;; subscription by combining a topic of conversation with a direction:
;; either publisher or subscriber. An endpoint combines a role with
;; handlers for events relating to the conversation"

(define-type Orientation (U 'publisher 'subscriber))

(struct: role ([orientation : Orientation]
	       [topic : Topic]
	       [interest-type : InterestType])
	 #:transparent)
(define-type Role role)

(define-type Message Topic) ;; Cheesy.

(define-type InterestType (U 'participant 'observer 'everything))

(define-type (Handler State) (TrapK EndpointEvent State))

(define-type (InterruptK State) (State -> (Transition State)))
(define-type (TrapK Event State) (Event -> (InterruptK State)))

(define-type EndpointEvent (U PresenceEvent
			      AbsenceEvent
			      MessageEvent))

(struct: presence-event ([role : Role]) #:transparent)
(struct: absence-event ([role : Role] [reason : Reason]) #:transparent)
(struct: message-event ([role : Role] [message : Message]) #:transparent)
(define-type PresenceEvent presence-event)
(define-type AbsenceEvent absence-event)
(define-type MessageEvent message-event)

(struct: (State)
	 transition ([state : State]
		     [actions : (ActionTree State)])
	 #:transparent)
(define-type (Transition State) (transition State))

(define-type (ActionTree State) (Constreeof (Action State)))

;; Existential quantification over State
(define-type CoTransition (All (Result) (All (State) (Transition State) -> Result) -> Result))

;; Specification of a new process
(struct: process-spec ([boot : (PID -> CoTransition)])
	 #:transparent)
(define-type ProcessSpec process-spec)

(define-type (PreAction State) (U (add-endpoint State)
				  delete-endpoint
				  send-message
				  (spawn State)
				  quit))

(struct: (State)
	 add-endpoint ([pre-eid : PreEID]
		       [role : Role]
		       [handler : (Handler State)])
	 #:transparent)
(define-type (AddEndpoint State) (add-endpoint State))

(struct: delete-endpoint ([pre-eid : PreEID]
			  [reason : Reason])
	 #:transparent)
(define-type DeleteEndpoint delete-endpoint)

(struct: send-message ([body : Message]
		       [orientation : Orientation])
	 #:transparent)
(define-type SendMessage send-message)

(struct: (State)
	 spawn ([spec : process-spec]
		[k : (Option (PID -> (InterruptK State)))]
		[debug-name : Any])
	 #:transparent)
(define-type (Spawn State) (spawn State))

(struct: quit ([pid : (Option PID)] ;; #f = suicide
	       [reason : Reason])
	 #:transparent)
(define-type Quit quit)

(define-type (Action State) (U (PreAction State)
			       (yield State)
			       (at-meta-level State)))

(struct: (State)
	 yield ([k : (InterruptK State)])
	 #:transparent)
(define-type (Yield State) (yield State))

(struct: (State)
	 at-meta-level ([preaction : (PreAction State)])
	 #:transparent)
(define-type (AtMetaLevel State) (at-meta-level State))

(define-type PID Number)

;;; Local Variables:
;;; eval: (put 'transition 'scheme-indent-function 1)
;;; eval: (put 'transition: 'scheme-indent-function 3)
;;; eval: (put 'transition/no-state 'scheme-indent-function 0)
;;; End:
