#lang typed/racket/base

(require racket/match)

(require (prefix-in core: "../main.rkt"))
(require "../sugar-typed.rkt")
(require "../vm.rkt")
(require "../process.rkt")
(require "../quasiqueue.rkt")

(provide debug)

(: debug : (All (ParentState) (Spawn ParentState) -> (Spawn ParentState)))
(define (debug spawn-child)
  (match-define (core:spawn child-spec parent-k debug-name) spawn-child)
  (core:spawn
   (process-spec
    (lambda: ([pid : PID]) ;; TODO: exploit this more in messages etc.
      (define original-cotransition ((process-spec-boot child-spec) pid))
      (: wrapped-cotransition : (All (R) (All (S) (Transition S) -> R) -> R))
      (define (wrapped-cotransition k)
	(: receiver : (All (S) (Transition S) -> R))
	(define (receiver child-transition)
	  ((inst k S) (wrap-transition debug-name child-transition)))
	((inst original-cotransition R) receiver))
      wrapped-cotransition))
   parent-k
   (list 'debug debug-name)))

(: wrap-transition : (All (ChildState) Any (Transition ChildState) -> (Transition ChildState)))
(define (wrap-transition debug-name child-transition)
  (match-define (core:transition child-state child-actions) child-transition)
  (log-debug "~v: New State ~v" debug-name child-state)
  (core:transition child-state ((inst action-tree-map ChildState)
				(wrap-action debug-name)
				child-actions)))

(: action-tree-map : (All (State) ((Action State) -> (Action State))
			  (ActionTree State)
			  -> (ActionTree State)))
(define (action-tree-map f actions)
  ((inst map (Action State) (Action State))
   f
   (quasiqueue->list (action-tree->quasiqueue actions))))

(: wrap-action : (All (ChildState) Any -> ((Action ChildState) -> (Action ChildState))))
(define ((wrap-action debug-name) action)
  (cond
   [(core:yield? action)
    (log-debug "~v: Yield" debug-name)
    (core:yield (wrap-interruptk debug-name (core:yield-k action)))]
   [(core:at-meta-level? action)
    (core:at-meta-level (wrap-preaction "Outer" debug-name (core:at-meta-level-preaction action)))]
   [else
    (wrap-preaction "Inner" debug-name action)]))

(: wrap-preaction : (All (ChildState) String Any (PreAction ChildState) -> (PreAction ChildState)))
(define (wrap-preaction level debug-name preaction)
  (match preaction
    [(core:add-endpoint pre-eid role handler)
     (log-debug "~v: ~a AddEndpoint ~v ~v" debug-name level pre-eid role)
     (core:add-endpoint pre-eid role (wrap-handler debug-name handler))]
    [(core:delete-endpoint pre-eid reason)
     (log-debug "~v: ~a DeleteEndpoint ~v ~v" debug-name level pre-eid reason)
     preaction]
    [(core:send-message body orientation)
     (log-debug "~v: ~a SendMessage ~v ~v" debug-name level body orientation)
     preaction]
    [(core:spawn spec maybe-k child-debug-name)
     (log-debug "~v: ~a Spawn ~v" debug-name level child-debug-name)
     (core:spawn spec (wrap-spawnk debug-name maybe-k) child-debug-name)]
    [(core:quit pid reason)
     (log-debug "~v: ~a Quit ~v ~v" debug-name level pid reason)
     preaction]))

(: wrap-interruptk : (All (ChildState) Any (InterruptK ChildState) -> (InterruptK ChildState)))
(define (wrap-interruptk debug-name ik)
  (lambda (state)
    (log-debug "~v: Old State ~v" debug-name state)
    (wrap-transition debug-name (ik state))))

(: wrap-spawnk : (All (ChildState)
		      Any
		      (Option (PID -> (InterruptK ChildState)))
		      -> (Option (PID -> (InterruptK ChildState)))))
(define (wrap-spawnk debug-name maybe-k)
  (and maybe-k
       (lambda: ([child-pid : PID]) (wrap-interruptk debug-name (maybe-k child-pid)))))

(: wrap-handler : (All (ChildState) Any (Handler ChildState) -> (Handler ChildState)))
(define (wrap-handler debug-name h)
  (lambda (event)
    (log-debug "~v: Incoming Event ~v" debug-name event)
    (wrap-interruptk debug-name (h event))))

