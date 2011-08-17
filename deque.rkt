#lang racket
(provide empty-deque empty-deque?
         front back pop-front push-back pop-back push-front  
         deque->list list->deque add-list-to-deque
         pop-func-push pop-func
         count count-qoq)
;implements an immutable deque with amoritized O(1) operations for 
; front , pop, push-back push-front
; using two lists: 'in' for pushing-back onto, 'out' for poping off of
; make (empty empty) the empty list
; if there is only one element x then it is stored as (empty 'x)
; which makes front easier

(struct deque (in out) #:transparent)

(define (empty-deque)
  (deque empty empty))

(define (empty-deque? q)
  (and (empty? (deque-out q)) (empty? (deque-in q))))

(define (front q)
  (if (empty-deque? q)
      #f
      (first (deque-out q))))

(define (back q)
  (if (empty-deque? q)
      #f
      (if (cons? (deque-in q))
          (first (deque-in q))
          (last  (deque-out q)))))

(define (pop-front q)
  (if (empty-deque? q)
      q
      (if (cons? (rest (deque-out q)))
          (deque (deque-in q) (rest (deque-out q)))
          (let ((kin (floor (/ (length(deque-in q)) 2))))
            (deque (take (deque-in q) kin)  
                   (reverse (drop (deque-in q) kin)))))))

; add pop-back
(define (pop-back q)
  (if (empty-deque? q)
      q 
      (if (not (empty? (deque-in q)))
          (deque (rest (deque-in q))  (deque-out q))
          (let ((kin (floor (/ (length(deque-out q)) 2))))
            (deque (rest (reverse (drop (deque-out q) kin)))  
                   (take (deque-out q) kin))))))

(define (push-back q x)
  (if (empty-deque? q)
      (deque empty (list x))
      (deque (cons x (deque-in q)) (deque-out q))))

(define (push-front q x)
  (deque (deque-in q) (cons x (deque-out q))))

(define (list->deque lst)
  (deque empty lst))

(define (deque->list q)
  (append (deque-out q) (reverse (deque-in q))))

(define (add-list-to-deque q lst)
  (deque (append (reverse lst) (deque-in q)) (deque-out q)))

(define (generic-pop-push-func q func next-deque)
  (if (empty-deque? q)
      (begin (func empty) q)
      (let ((q2 (next-deque q)))
        (if (empty? (deque-out q2))
            (deque empty (func (reverse (deque-in q2))))
            q2))))

(define (pop-func q func)
  (generic-pop-push-func 
   q func (lambda (x) #| just pop |# 
            (deque 
             (deque-in x) 
             (rest (deque-out x))))))

(define (pop-func-push q func)
  (generic-pop-push-func
   q func (lambda(x) #| pop then push-back |# 
            (deque 
             (cons (first (deque-out q)) (deque-in q)) 
             (rest (deque-out q))))))


(define (count q)
  (+ (length (deque-in q)) 
     (length (deque-out q))))

(define (count-qoq q)
  (+  (apply + 0 (map count (deque-in q))) 
      (apply + 0 (map count (deque-out q)))))


(define q1 (list->deque (list 1 2 3 4 5)))
q1
(front q1)
(back q1)
(define q2 (pop-front q1))
q2
(front q2)
(back q2)
(define q3 (pop-back q2))
q3
(front q3)
(back q3)
(define q4 (pop-back q3))
q4
(front q4)
(back q4)
(define q5 (pop-back q4))
q5
(front q5)
(back q5)
(define q6 (push-front q5 100))
q6
(front q6)
(back q6)
(define q7 (push-front q6 101))
q7
(front q7)
(back q7)
(define q8 (pop-back q7))
q8
(front q8)
(back q8)

