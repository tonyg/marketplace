#lang racket/base

(require feature-profile/plug-in-lib
         (only-in profile/render-text render)
         (only-in profile/analyzer analyze-samples)
         "process.rkt")

(provide marketplace-feature feature?)

(define marketplace-feature
  (feature "Marketplace" marketplace-continuation-mark-key values ; no grouper. no basic analysis.
           (lambda (f-p)
             (define intern (make-interner))
             ;; add thread id and timestamp back on each core sample
             (define post-processed
               (for/list ([c-s (feature-report-core-samples f-p)]
                          [p-s (cdr (feature-report-raw-samples f-p))])
                 ;; process identifiers are the full ancestry of a process,
                 ;; starting at the ground VM. computed from core samples.
                 (define processed
                   (let loop ([vs (filter values c-s)]) ; remove absent marks
                     (if (null? vs) '(ground) (cons vs (loop (cdr vs))))))
                 (list* (car p-s) (cadr p-s) ; thread id + timestamp
                        (for/list ([v processed])
                          ;; analyzer expects (id . srcloc) pairs
                          ;; car may not actually be an id, but that's ok
                          (intern (cons v #f))))))
             ;; call the edge profiler
             (newline) (newline) (displayln "Marketplace Processes\n")
             (render (analyze-samples (cons (feature-report-total-time f-p) post-processed))))))
