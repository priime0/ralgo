#lang racket

;; Implementation of a Union Find Disjoint Set / Disjoint Set Union

(provide (all-defined-out))

;; A [UFDS X] is a collection of disjoint sets, where
;; - `parents` is a [Vectorof X] representing the parents / representatives of
;;   the given index, which can be itself.
;; - `ranks` is a [Vectorof X] representing the depth of the tree for use in
;;   optimization of unioning sets.
(struct ufds (parents ranks) #:transparent)

;; Create a new UFDS
(define (make-ufds num)
  (integer? . -> . ufds?)
  (ufds (build-vector num identity)
        (build-vector num identity)))

;; Find the set that the given index belongs to
(define (ufds-find ds num)
  (ufds? integer? . -> . integer?)
  (define parents (ufds-parents ds))
  (define current-index (vector-ref parents num))
  (if (= num current-index)
      num
      (ufds-find ds current-index)))

;; Find the set that the given index belongs to, and optimize the current tree
(define (ufds-find! ds num)
  (ufds? integer? . -> . integer?)
  (define parents (ufds-parents ds))
  (define current-index (vector-ref parents num))
  (if (= num current-index)
      num
      ;; Compress the path and return the answer
      (let ([root (ufds-find! ds current-index)])
        (vector-set! parents num root)
        root)))

;; Do the two indices belong to the same set?
(define (ufds-same-set? ds num1 num2)
  (ufds? integer? integer? . -> . boolean?)
  (= (ufds-find ds num1)
     (ufds-find ds num2)))

;; Union two sets given by their indices
;; SIDE EFFECT: Updates the representatives field to union the two indices
(define (ufds-union! ds num1 num2)
  (ufds? integer? integer? . -> . ufds?)
  (define parents (ufds-parents ds))
  (define ranks (ufds-ranks ds))
  (define a (ufds-find! ds num1))
  (define b (ufds-find! ds num2))
  (define rank-a (vector-ref ranks a))
  (define rank-b (vector-ref ranks b))
  (cond [(= a b) ds]
        [(< rank-a rank-b)
         (vector-set! parents a b)
         ds]
        [else
         (vector-set! parents b a)
         (when (= rank-a rank-b)
           (vector-set! ranks a (add1 rank-a)))
         ds]))

(module+ test
  (require rackunit)

  (define ufds1 (make-ufds 5))

  (check-false (ufds-same-set? ufds1 0 1))
  (check-false (ufds-same-set? ufds1 1 2))
  (check-false (ufds-same-set? ufds1 3 4))

  (void (ufds-union! ufds1 1 3))

  (check-true (ufds-same-set? ufds1 1 3))
  (check-false (ufds-same-set? ufds1 0 1))
  (check-false (ufds-same-set? ufds1 1 2))
  (check-false (ufds-same-set? ufds1 3 4))

  (void (ufds-union! ufds1 3 0))

  (check-true (ufds-same-set? ufds1 1 3))
  (check-true (ufds-same-set? ufds1 3 1))
  (check-true (ufds-same-set? ufds1 0 1))
  (check-false (ufds-same-set? ufds1 3 4))
  (check-false (ufds-same-set? ufds1 1 2))
  (check-false (ufds-same-set? ufds1 2 4))

  (void (ufds-union! ufds1 2 4))

  (check-true (ufds-same-set? ufds1 2 4))
  (check-false (ufds-same-set? ufds1 2 3))

  (void (ufds-union! ufds1 0 2))

  (check-true (ufds-same-set? ufds1 0 1))
  (check-true (ufds-same-set? ufds1 1 2))
  (check-true (ufds-same-set? ufds1 2 3))
  (check-true (ufds-same-set? ufds1 3 4)))
