#lang racket/base

(require (for-syntax racket/base))
(require "support/dsl-untyped.rkt")

;; We define and provide these here so that they can be used by both
;; typed and untyped contexts. If we define them separately in untyped
;; and typed contexts, then TR's wrapping of provided identifiers
;; interferes with literal comparison in our macros. See also
;; definition and use of the file support/dsl-typed.rkt in git rev
;; b477046.

(define&provide-dsl-helper-syntaxes "endpoint definition context"
  [match-state
   match-orientation
   match-conversation
   match-interest-type
   match-reason
   on-presence
   on-absence
   on-message])
