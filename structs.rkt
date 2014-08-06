#lang racket/base

(provide (all-defined-out))

;; (define-type Orientation (U 'publisher 'subscriber))

(struct role (orientation ;; Orientation
	      topic ;; Topic
	      interest-type ;; InterestType
	      )
	#:transparent)

;; (define-type Message Topic) ;; Cheesy.

;; (define-type InterestType (U 'participant 'observer 'everything))

;; (define-type (Handler State) (TrapK EndpointEvent State))

;; (define-type (InterruptK State) (State -> (Transition State)))
;; (define-type (TrapK Event State) (Event -> (InterruptK State)))

;; (define-type EndpointEvent (U PresenceEvent
;; 			      AbsenceEvent
;; 			      MessageEvent))

(struct presence-event (role) #:transparent)
(struct absence-event (role reason) #:transparent)
(struct message-event (role message) #:transparent)

(struct transition (state ;; State
		    actions ;; (ActionTree State)
		    )
	#:transparent)

;; (define-type (ActionTree State) (Constreeof (Action State)))

;; Existential quantification over State
;; (define-type CoTransition (All (Result) (All (State) (Transition State) -> Result) -> Result))

;; Specification of a new process
(struct process-spec (boot ;; (PID -> CoTransition)
		      )
	#:transparent)
;; (define-type ProcessSpec process-spec)

;; (define-type (PreAction State) (U (add-endpoint State)
;; 				  delete-endpoint
;; 				  send-message
;; 				  (spawn State)
;; 				  quit))

(struct add-endpoint (pre-eid ;; PreEID
		      role ;; Role
		      handler ;; (Handler State)
		      )
	#:transparent)

(struct delete-endpoint (pre-eid ;; PreEID
			 reason ;; Reason
			 )
	#:transparent)

(struct send-message (body ;; Message
		      orientation ;; Orientation
		      )
	#:transparent)

(struct spawn (spec ;; process-spec
	       k ;; (Option (PID -> (InterruptK State)))
	       debug-name ;; Any
	       )
	#:transparent)

(struct quit (pid ;; (Option PID) ;; #f = suicide
	      reason ;; Reason
	      )
	#:transparent)

;; (define-type (Action State) (U (PreAction State)
;; 			       (yield State)
;; 			       (at-meta-level State)))

(struct yield (k ;; (InterruptK State)
	       )
	#:transparent)

(struct at-meta-level (preaction ;; (PreAction State)
		       )
	#:transparent)

;; (define-type PID Number)

;;; Local Variables:
;;; eval: (put 'transition 'scheme-indent-function 1)
;;; eval: (put 'transition/no-state 'scheme-indent-function 0)
;;; End:
