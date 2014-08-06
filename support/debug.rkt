#lang racket/base

(require racket/match)

(require (prefix-in core: "../main.rkt"))
(require "../sugar.rkt")
(require "../vm.rkt")
(require "../process.rkt")
(require "../quasiqueue.rkt")

(require "gui.rkt")

;; (define-type Debugger (All (S) (S -> S)))

(provide debug)

;; debug : (All (ParentState) (Spawn ParentState) -> (Spawn ParentState))
(define (debug spawn-child)
  (match-define (core:spawn child-spec parent-k debug-name) spawn-child)
  (core:spawn
   (core:process-spec
    (lambda (pid) ;; TODO: exploit this more in messages etc.
      (define original-cotransition ((core:process-spec-boot child-spec) pid))
      ;; wrapped-cotransition : (All (R) (All (S) (Transition S) -> R) -> R)
      (define (wrapped-cotransition k)
	;; receiver : (All (S) (Transition S) -> R)
	(define (receiver child-transition)
	  (define d (open-debugger debug-name))
	  (k (wrap-transition d child-transition)))
	(original-cotransition receiver))
      wrapped-cotransition))
   parent-k
   (list 'debug debug-name)))

;; wrap-transition : (All (ChildState)
;; 			  Debugger
;; 			  (Transition ChildState)
;; 			  -> (Transition ChildState))
(define (wrap-transition d child-transition0)
  (define child-transition (d child-transition0))
  (match-define (core:transition child-state child-actions) child-transition)
  (core:transition child-state (action-tree-map (wrap-action d)
						child-actions)))

;; action-tree-map : (All (State) ((Action State) -> (Action State))
;; 			  (ActionTree State)
;; 			  -> (ActionTree State))
(define (action-tree-map f actions)
  (map f (quasiqueue->list (action-tree->quasiqueue actions))))

;; wrap-action : (All (ChildState)
;; 		      Debugger
;; 		      -> ((Action ChildState) -> (Action ChildState)))
(define ((wrap-action d) action)
  (cond
   [(core:yield? action)
    (core:yield (wrap-interruptk d (core:yield-k action)))]
   [(core:at-meta-level? action)
    (core:at-meta-level (wrap-preaction #t d (core:at-meta-level-preaction action)))]
   [else
    (wrap-preaction #f d action)]))

;; wrap-preaction : (All (ChildState)
;; 			 Boolean
;; 			 Debugger
;; 			 (PreAction ChildState)
;; 			 -> (PreAction ChildState))
(define (wrap-preaction meta? d preaction)
  (match preaction
    [(core:add-endpoint pre-eid role handler)
     (core:add-endpoint pre-eid role (wrap-handler meta? d handler))]
    [(core:delete-endpoint pre-eid reason)
     preaction]
    [(core:send-message body orientation)
     preaction]
    [(core:spawn spec maybe-k child-debug-name)
     (core:spawn spec (wrap-spawnk d maybe-k) child-debug-name)]
    [(core:quit pid reason)
     preaction]))

;; wrap-interruptk : (All (ChildState)
;; 			  Debugger
;; 			  (InterruptK ChildState)
;; 			  -> (InterruptK ChildState))
(define (wrap-interruptk d ik)
  (lambda (state)
    (wrap-transition d (ik state))))

;; wrap-spawnk : (All (ChildState)
;; 		      Debugger
;; 		      (Option (PID -> (InterruptK ChildState)))
;; 		      -> (Option (PID -> (InterruptK ChildState))))
(define (wrap-spawnk d maybe-k)
  (and maybe-k
       (lambda (child-pid) (wrap-interruptk d (maybe-k child-pid)))))

;; wrap-handler : (All (ChildState)
;; 		       Boolean
;; 		       Debugger
;; 		       (Handler ChildState)
;; 		       -> (Handler ChildState))
(define (wrap-handler meta?0 d h)
  (lambda (event0)
    (match-define (cons meta? event) (d (cons meta?0 event0)))
    (wrap-interruptk d (h event))))
