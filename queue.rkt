#lang racket
(provide empty-queue empty-queue?
         top pop push-back  push-front  
         queue->list list->queue add-list-to-queue
         pop-func-push pop-func
         count count-qoq)
;implements an immutable queue with amoritized O(1) operations for 
; top , pop, push-back
; using two lists: 'in' for pushing onto, 'out' for poping off of
; make (empty empty) the empty list
; if there is only one element x then it is stored as (empty 'x)

(struct queue (in out) #:transparent)

(define (empty-queue)
  (queue empty empty))

(define (empty-queue? q)
  (and (empty? (queue-out q)) (empty? (queue-in q))))

(define (top q)
  (if (empty-queue? q)
      #f
      (first (queue-out q))))

(define (pop q)
  (if (empty-queue? q)
      (error "popping and empty list")
      (if (cons? (rest (queue-out q)))
          (queue (queue-in q) (rest (queue-out q)))
          (let* ((len (length(queue-in q)))
                 (kin (floor (/ len 2))))
            (queue 
             (take (queue-in q) kin)  
             (reverse (drop (queue-in q) kin))))))) 

(define (push-back q x)
  (if (empty-queue? q)
      (queue empty (list x))
      (queue (cons x (queue-in q)) (queue-out q))))

(define (push-front q x)
  (queue (queue-in q) (cons x (queue-out q))))

(define (list->queue lst)
  (queue empty lst))

(define (queue->list q)
   (append (queue-out q) (reverse (queue-in q))))

(define (add-list-to-queue q lst)
  (queue (append (reverse lst) (queue-in q)) (queue-out q)))

(define (pop-func q func)
  (if (empty-queue? q)
      (begin (func empty) q)
      (let ((q2 (queue (queue-in q)
                       (rest (queue-out q)))))
        (if (empty? (queue-out q2))
            (queue empty (func (queue-in q2)))
            q2))))

(define (pop-func-push q func)
  #;(printf "pop-push (~a,~a)\n" (length (queue-in q)) 
          (length (queue-out q)))
  (if (empty-queue? q)
      (begin (func empty) q)
      (let ((q2 (queue (cons (first (queue-out q)) (queue-in q))
                       (rest (queue-out q)))))
        (if (empty? (queue-out q2))
            (queue empty (func (queue-in q2)))
            q2))))

(define (count q)
  (+ (length (queue-in q)) 
     (length (queue-out q))))

(define (count-qoq q)
  (+  (apply + 0 (map count (queue-in q))) 
      (apply + 0 (map count (queue-out q)))))

