#lang typed/racket/base
;; Virtualized operating system, this time with presence and types.

;; TODO: contracts for State checking
;; TODO: types for Message and MetaMessage (will require rethinking at-meta-level spawn)
;; TODO: revisit exposure of PIDs to processes.
;;        - make processes parametric in the PID type?
;;        - simply make PIDs unavailable to processes?
;;        - revisit points-of-attachment idea, and expose presence on PIDs properly?

(require racket/match)
(require "types.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "actions.rkt")
(require "nested.rkt")
(require "ground.rkt")
(require (rename-in "tr-struct-copy.rkt" [tr-struct-copy struct-copy])) ;; PR13149 workaround

(require/typed "unify.rkt"
	       [opaque Wild wild?]
	       [wild (case-> (-> Wild) (Symbol -> Wild))]
	       [non-wild? (Any -> Boolean)]
	       [ground? (Any -> Boolean)])

(provide (all-from-out "types.rkt")
	 (all-from-out "roles.rkt")
	 make-nested-vm
	 run-ground-vm

	 Wild
	 wild
	 wild?
	 non-wild?
	 ground?)
