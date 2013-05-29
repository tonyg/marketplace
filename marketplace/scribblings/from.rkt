#lang racket/base
;; Provides |from|, useful for importing identifiers from non-default
;; contexts without spelling them differently.

(require scribble/decode)

(provide from)

(define-syntax-rule (from require-spec pre-content ...)
  (let ()
    (local-require require-spec)
    (splice (list pre-content ...))))
