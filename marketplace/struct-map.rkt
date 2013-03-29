#lang racket/base

(provide prop:struct-map
	 struct-mappable?
	 extract-struct-mapper
	 struct-map
	 struct-map/accumulator)

(define-values (prop:struct-map struct-mappable? extract-struct-mapper)
  (make-struct-type-property 'struct-map))

;; (X -> Y) Struct<X> -> Struct<Y>
(define (struct-map f x)
  (define-values (result acc)
    (struct-map* 'struct-map (lambda (v acc) (values (f v) acc)) (void) x))
  result)

;; (X Seed -> Y Seed) Seed Struct<X> -> Struct<Y> Seed
(define (struct-map/accumulator f seed x)
  (struct-map* 'struct-map/accumulator f seed x))

(define (struct-map* name f seed x)
  (define m (cond
	     [(struct-mappable? x) (extract-struct-mapper x)]
	     [(prefab-struct-key x) => prefab-struct-mapper]
	     [else (error name "No struct-map property or mapper for ~v" x)]))
  (m f seed x))

(define ((prefab-struct-mapper key) f initial-seed x)
  (define-values (new-fields final-seed)
    (for/fold ([new-fields '()] [old-seed initial-seed])
	([old-field (cdr (vector->list (struct->vector x)))])
      (define-values (new-field new-seed) (f old-field old-seed))
      (values (cons new-field new-fields) new-seed)))
  (values (apply make-prefab-struct key (reverse new-fields)) final-seed))
