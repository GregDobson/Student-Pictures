#lang racket
(provide empty-queue empty-queue?
         top bottom pop push-back pop-back push-front  
         queue->list list->queue add-list-to-queue
         pop-func-push pop-func
         count count-qoq)
;implements an immutable queue with amoritized O(1) operations for 
; top , pop, push-back push-front
; using two lists: 'in' for pushing-back onto, 'out' for poping off of
; make (empty empty) the empty list
; if there is only one element x then it is stored as (empty 'x)
; which makes top easier

(struct queue (in out) #:transparent)

(define (empty-queue)
  (queue empty empty))

(define (empty-queue? q)
  (and (empty? (queue-out q)) (empty? (queue-in q))))

(define (top q)
  (if (empty-queue? q)
      #f
      (first (queue-out q))))

(define (bottom q)
  (if (empty-queue? q)
      #f
      (if (cons? (queue-in q))
          (first (queue-in q))
          (first (reverse (queue-out q))))))

(define (pop q)
  (if (empty-queue? q)
      (error "popping and empty list")
      (if (cons? (rest (queue-out q)))
          (queue (queue-in q) (rest (queue-out q)))
          (let ((kin (floor (/ (length(queue-in q)) 2))))
            (queue (take (queue-in q) kin)  
                   (reverse (drop (queue-in q) kin)))))))

; add pop-back
(define (pop-back q)
  (if (empty-queue? q)
      (error "popping and empty list") 
      (if (not (empty? (queue-in q)))
          (queue (rest (queue-in q))  (queue-out q))
          (let ((kin (floor (/ (length(queue-out q)) 2))))
            (queue (rest (reverse (drop (queue-out q) kin)))  
                   (take (queue-out q) kin))))))

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

(define (generic-pop-push-func q func next-queue)
  (if (empty-queue? q)
      (begin (func empty) q)
      (let ((q2 (next-queue q)))
        (if (empty? (queue-out q2))
            (queue empty (func (reverse (queue-in q2))))
            q2))))

(define (pop-func q func)
  (generic-pop-push-func 
   q func (lambda (x) #| just pop |# 
            (queue 
             (queue-in x) 
             (rest (queue-out x))))))

(define (pop-func-push q func)
  (generic-pop-push-func
   q func (lambda(x) #| pop then push-back |# 
            (queue 
             (cons (first (queue-out q)) (queue-in q)) 
             (rest (queue-out q))))))


(define (count q)
  (+ (length (queue-in q)) 
     (length (queue-out q))))

(define (count-qoq q)
  (+  (apply + 0 (map count (queue-in q))) 
      (apply + 0 (map count (queue-out q)))))

#|
(define q1 (list->queue (list 1 2 3 4 5)))
q1
(top q1)
(bottom q1)
(define q2 (pop q1))
q2
(top q2)
(bottom q2)
(define q3 (pop-back q2))
q3
(top q3)
(bottom q3)
(define q4 (pop-back q3))
q4
(top q4)
(bottom q4)
(define q5 (pop-back q4))
q5
(top q5)
(bottom q5)
(define q6 (push-front q5 100))
q6
(top q6)
(bottom q6)
(define q7 (push-front q6 101))
q7
(top q7)
(bottom q7)
(define q8 (pop-back q7))
q8
(top q8)
(bottom q8)
|#
