#lang typed/racket/base

(provide QuasiQueue
	 Constreeof
	 empty-quasiqueue
	 quasiqueue-empty?
	 quasiqueue-append-list
	 quasiqueue-append
	 quasiqueue
	 list->quasiqueue
	 quasiqueue->list
	 quasiqueue->cons-tree)

;; A QuasiQueue<X> is a list of Xs in *reversed* order.
(define-type (QuasiQueue X) (Listof X))

(define-type (Constreeof X) (Rec CT (U X (Pairof CT CT) False Void Null)))

(: empty-quasiqueue : (All (X) -> (QuasiQueue X)))
(define (empty-quasiqueue) '())

(: quasiqueue-empty? : (All (X) (QuasiQueue X) -> Boolean))
(define (quasiqueue-empty? q) (null? q))

(: quasiqueue-append-list : (All (X) (QuasiQueue X) (Listof X) -> (QuasiQueue X)))
(define (quasiqueue-append-list q xs)
  (append (reverse xs) q))

(: quasiqueue-append : (All (X) (QuasiQueue X) (QuasiQueue X) -> (QuasiQueue X)))
(define (quasiqueue-append q1 q2)
  (append q2 q1))

(: quasiqueue : (All (X) X * -> (QuasiQueue X)))
(define (quasiqueue . xs)
  (reverse xs))

(: list->quasiqueue : (All (X) (Listof X) -> (QuasiQueue X)))
(define (list->quasiqueue xs)
  (reverse xs))

(: quasiqueue->list : (All (X) (QuasiQueue X) -> (Listof X)))
(define (quasiqueue->list q)
  (reverse q))

(: quasiqueue->cons-tree : (All (X) (QuasiQueue X) -> (Constreeof X)))
(define (quasiqueue->cons-tree q)
  ;; (reverse q) -- can't use this, TR won't prove Listof X <: Constreeof X.
  (let loop ((#{acc : (Constreeof X)} '()) (q q))
    (if (null? q)
	acc
	(loop (cons (car q) acc) (cdr q)))))
