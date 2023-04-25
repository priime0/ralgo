#lang racket

;; Implementation of Option

(provide (all-defined-out))

;; An [Option X] is one of:
;; - None
;; - [Some X]
;; and is used to represent a Maybe value -- a value that may be something or
;; nothing.
(struct none () #:transparent)
(struct some ([val #:mutable]) #:transparent)

;; Defines a contract that determines whether the given argument is an option
(define option? (or/c none? some?))

;; Returns the first option `opt` if it is a `some?` value, else returns the
;; second option `optb`. The second argument is eagerly evaluated; consider
;; using `option-or-else` if the evaluation of `optb` is inefficient.
(define (option-or opt optb)
  (option? option? . -> . option?)
  (if (some? opt) opt optb))

;; Returns `opt` if it is a `some?` value, else returns the value produced by
;; the `producer` function.
(define (option-or-else opt producer)
  (option? (-> option?) . -> . option?)
  (if (some? opt) opt (producer)))

;; Unwrap the value contained in the option if it is a `some?`, else surface
;; an error.
(define (option-unwrap opt)
  (option? . -> . any)
  (match opt
    [(some x) x]
    [_ (error 'option-unwrap "unwrapped an empty option")]))

;; Unwrap the value contained in the option if it is a `some?`, else return
;; the `default` value.
(define (option-unwrap-or opt default)
  (option? any/c . -> . any)
  (match opt
    [(some x) x]
    [_ default]))

;; Unwrap the value contained in the option if it is a `some?`, else return
;; the value produced by the `producer` function.
(define (option-unwrap-or-else opt producer)
  (option? (-> any) . -> . any)
  (match opt
    [(some x) x]
    [_ (producer)]))

(module+ test
  (require rackunit)

  ;; `option?` pass
  (check-true (option? (none)))
  (check-true (option? (some 3)))
  (check-true (option? (some "hello")))

  ;; `option?` fail
  (check-false (option? '()))
  (check-false (option? #f))
  (check-false (option? 0))
  (check-false (option? ""))

  ;; `option-or`
  (check-equal? (option-or (some 3) (some 4)) (some 3))
  (check-equal? (option-or (none) (some "hello")) (some "hello"))
  (check-equal? (option-or (some #t) (none)) (some #t))
  (check-equal? (option-or (none) (none)) (none))

  ;; `option-or-else`
  (check-equal? (option-or-else (some 1) (thunk (some 2))) (some 1))
  (check-equal? (option-or-else (none) (thunk (some 3))) (some 3))
  (check-equal? (option-or-else (some 2) (thunk (none))) (some 2))
  (check-equal? (option-or-else (none) (thunk (none))) (none))

  ;; `option-unwrap`
  (check-equal? (option-unwrap (some 2)) 2)
  (check-exn exn:fail? (thunk (option-unwrap (none))))

  ;; `option-unwrap-or`
  (check-equal? (option-unwrap-or (some 0) -1) 0)
  (check-equal? (option-unwrap-or (none) "hello") "hello")

  ;; `option-unwrap-or-else`
  (check-equal? (option-unwrap-or-else (some 10) (thunk 5)) 10)
  (check-equal? (option-unwrap-or-else (none) (thunk "hello")) "hello"))
