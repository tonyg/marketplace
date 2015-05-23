#lang setup/infotab
(define scribblings '(("scribblings/marketplace.scrbl" (multi-page))))
(define deps '("base"
               "data-lib"
               "gui-lib"
               "images-lib"
               "rackunit-lib"))
(define build-deps '("scribble-lib"
                     "slideshow-lib"
                     "typed-racket-lib"))
