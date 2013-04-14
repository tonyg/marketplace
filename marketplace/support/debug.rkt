#lang typed/racket/base

(require racket/match)

(require (prefix-in core: "../main.rkt"))
(require "../sugar-typed.rkt")
(require "../vm.rkt")
(require "../process.rkt")
(require "../quasiqueue.rkt")

(require/typed "gui.rkt"
	       [opaque AnyState any-state?]
	       [open-debugger (Any -> (Values (AnyState -> Void) (-> Void)))])

(provide debug)

(struct: debugger ([out : (AnyState -> Void)] [in : (-> Void)]))

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
	  (define-values (send-to-debugger! receive-from-debugger!)
	    (open-debugger debug-name))
	  (define d (debugger send-to-debugger! receive-from-debugger!))
	  ((inst k S) (wrap-transition d child-transition)))
	((inst original-cotransition R) receiver))
      wrapped-cotransition))
   parent-k
   (list 'debug debug-name)))

(: wrap-transition : (All (ChildState)
			  debugger
			  (Transition ChildState)
			  -> (Transition ChildState)))
(define (wrap-transition d child-transition)
  ((debugger-out d) (cast child-transition AnyState))
  ((debugger-in d))
  (match-define (core:transition child-state child-actions) child-transition)
  (core:transition child-state ((inst action-tree-map ChildState)
				(wrap-action d)
				child-actions)))

(: action-tree-map : (All (State) ((Action State) -> (Action State))
			  (ActionTree State)
			  -> (ActionTree State)))
(define (action-tree-map f actions)
  ((inst map (Action State) (Action State))
   f
   (quasiqueue->list (action-tree->quasiqueue actions))))

(: wrap-action : (All (ChildState)
		      debugger
		      -> ((Action ChildState) -> (Action ChildState))))
(define ((wrap-action d) action)
  (cond
   [(core:yield? action)
    (core:yield (wrap-interruptk d (core:yield-k action)))]
   [(core:at-meta-level? action)
    (core:at-meta-level (wrap-preaction #t d (core:at-meta-level-preaction action)))]
   [else
    (wrap-preaction #f d action)]))

(: wrap-preaction : (All (ChildState)
			 Boolean
			 debugger
			 (PreAction ChildState)
			 -> (PreAction ChildState)))
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

(: wrap-interruptk : (All (ChildState)
			  debugger
			  (InterruptK ChildState)
			  -> (InterruptK ChildState)))
(define (wrap-interruptk d ik)
  (lambda (state)
    (wrap-transition d (ik state))))

(: wrap-spawnk : (All (ChildState)
		      debugger
		      (Option (PID -> (InterruptK ChildState)))
		      -> (Option (PID -> (InterruptK ChildState)))))
(define (wrap-spawnk d maybe-k)
  (and maybe-k
       (lambda: ([child-pid : PID]) (wrap-interruptk d (maybe-k child-pid)))))

(: wrap-handler : (All (ChildState)
		       Boolean
		       debugger
		       (Handler ChildState)
		       -> (Handler ChildState)))
(define (wrap-handler meta? d h)
  (lambda (event)
    ((debugger-out d) (cast (cons meta? event) AnyState))
    (wrap-interruptk d (h event))))
