#lang racket/base

(require racket/contract
         racket/list)


;; Implementation of a simple immutable queue in Racket

(provide (all-defined-out))

;; A [Queue X] is a collection of X that maintain the ordering when inserting
;; new elements or popping existing ones, where:
;; - `front` is a [Listof X]
;; - `back` is a [Listof X]
(struct queue (front back) #:transparent)

;; Create an empty Queue
(define/contract (empty-queue)
  (-> queue?)
  (queue '() '()))

;; Transform a list into a queue
(define/contract (list->queue l)
  (list? . -> . queue?)
  (queue l '()))

;; Transform the queue into a list
(define/contract (queue->list q)
  (queue? . -> . list?)
  (define front (queue-front q))
  (define back (queue-back q))
  (append front (reverse back)))

;; Add an element to the queue
(define/contract (queue-add q x)
  (queue? any/c . -> . queue?)
  (struct-copy queue q [back (cons x (queue-back q))]))

;; Pop an element from the queue, returning the new queue
(define/contract (queue-pop q)
  (queue? . -> . queue?)
  (define front (queue-front q))
  (define back (queue-back q))
  (cond
    [(cons? front) (queue (rest front) back)]
    [(cons? back) (queue (rest (reverse back)) '())]
    [else (error "Popped an empty queue")]))

;; Retrieve the element at the front of the queue
(define/contract (queue-top q)
  (queue? . -> . any)
  (define front (queue-front q))
  (define back (queue-back q))
  (cond
    [(cons? front) (first front)]
    [(cons? back) (last back)]
    [else (error "No top for empty queue")]))

;; Is the queue empty?
(define/contract (queue-empty? q)
  (queue? . -> . boolean?)
  (and (empty? (queue-front q)) (empty? (queue-back q))))

;; Determine the length of the queue
(define/contract (queue-length q)
  (queue? . -> . exact-nonnegative-integer?)
  (+ (length (queue-front q)) (length (queue-back q))))

;; Does the queue contain the given element?
(define/contract (queue-contains? q x)
  (queue? any/c . -> . boolean?)
  (define front (queue-front q))
  (define back (queue-back q))
  (or (cons? (member x front)) (cons? (member x back))))


(module+ test
  (require rackunit)

  (define eq (empty-queue))

  ;; Adding to queue
  (check-true (queue-empty? eq))
  (check-false (queue-empty? (queue-add eq 1)))
  (check-equal? 3 (queue-length (queue-add (queue-add (queue-add eq 1) 2) 3)))

  ;; Popping from queue
  (check-true (queue-empty? (queue-pop (queue-add eq 1))))
  (check-equal? 1 (queue-length (queue-pop (queue-add (queue-add eq 1) 2))))

  ;; List to queue and queue to list
  (check-equal? '(1 2 3 4 5) (queue->list (list->queue '(1 2 3 4 5))))
  (check-equal? '(2 3 4 5) (queue->list (queue-pop (list->queue '(1 2 3 4 5)))))
  (check-equal?
   '(2 3 4 5 6)
   (queue->list (queue-add (queue-pop (list->queue '(1 2 3 4 5))) 6)))
  (check-equal?
   '()
   (queue->list
    (queue-pop (queue-pop (queue-pop (queue-pop (queue-pop (list->queue
                                                            '(1 2 3 4 5)))))))))

  ;; Queue contains
  (check-true (queue-contains? (list->queue '(1 2 3 4 5)) 3))
  (check-true (queue-contains? (queue-add (list->queue '(1 2 3 4 5)) 6) 6))
  (check-false (queue-contains? (queue-add (list->queue '(1 2 3)) 4) 5))

  ;; Queue top
  (check-equal? 1 (queue-top (list->queue '(1 2 3))))
  (check-equal? 2 (queue-top (queue-pop (list->queue '(1 2 3))))))
