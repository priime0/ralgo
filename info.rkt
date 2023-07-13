#lang info

(define deps '("base" "rackunit"))
(define build-deps '("scribble-lib"
                     "scribble-text-lib"
                     "racket-doc"
                     "rackunit-doc"))

(define name "ralgo")
(define collection 'multi)
(define pkg-desc "Convenient implementations of various data structures for Racket")
(define pkg-authors '(priime0))
(define homepage "https://github.com/priime0/ralgo")
(define version "0.1")
(define license 'MIT)

(define test-include-paths 'all)
