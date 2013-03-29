#lang typed/racket/base

(provide list-max)

(: list-max : (Listof Integer) -> Integer)
(define (list-max xs)
  (foldr max 0 xs))
