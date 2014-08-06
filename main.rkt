#lang racket/base
;; Virtualized operating system, this time with presence.

;; TODO: contracts for State checking
;; TODO: revisit exposure of PIDs to processes.
;;        - make processes parametric in the PID type?
;;        - simply make PIDs unavailable to processes?
;;        - revisit points-of-attachment idea, and expose presence on PIDs properly?

(require racket/match)
(require "structs.rkt")
(require "roles.rkt")
(require "vm.rkt")
(require "actions.rkt")
(require "nested.rkt")
(require "ground.rkt")
(require "unify.rkt")

(provide (all-from-out "structs.rkt")
	 (all-from-out "roles.rkt")
	 make-nested-vm
	 run-ground-vm

	 wild
	 wild?
	 non-wild?
	 ground?)
