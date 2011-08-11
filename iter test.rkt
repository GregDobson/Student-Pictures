#lang racket
(define test (build-list 5 identity))
test
(define (make-iterator lst func)
  (define remaining lst)
  (lambda ()
    (define result (first remaining))
    (set! remaining (rest remaining))
    (when (empty? remaining)
      (set! remaining (func lst)))
    result))

(define iter (make-iterator test))

#|
(iter)
(iter)
(iter)
(iter)
(iter)
(iter)
(iter)
(iter)
(iter)
(iter)
(iter)
(iter)
|#

(define (make-shuffle-iterator lst)
  (define remaining lst)
  (lambda ()
    (define result (first remaining))
    (set! remaining (rest remaining))
    (when (empty? remaining)
      (set! remaining (shuffle lst)))
    result))

(define s-iter (make-iterator test))
(s-iter)
(s-iter)
(s-iter)
(s-iter)
(s-iter)
(s-iter)
(s-iter)
(s-iter)
(s-iter)
(s-iter)
(s-iter)



