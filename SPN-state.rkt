#lang racket
(require 2htdp/batch-io racket/gui/base framework
         (prefix-in gen: racket/generator) "deque.rkt")
(provide compute-state)

;routine for formin or reforming groups
(define (list->qoq lst ngroups)
  ;helpers
  (define (list->short-lists 
           the-list 
           num-lists)
    (let*  ((total (length the-list)) 
            (chunk (floor (/ total num-lists))))
      (if (= total chunk) 
          (list the-list)
          (cons (take the-list chunk) 
                (list->short-lists 
                 (drop the-list chunk) 
                 (sub1 num-lists))))))
  
  (define (lol->qoq lol)
    (list->deque  (map list->deque lol)))
  
  ;computation
  (lol->qoq (list->short-lists lst ngroups)))

;routine used to reform groups into ngroups
(define (qoq->qoq the-qoq ngroups)
  ;helpers
  (define (qoq->list qoq)
    (append* (map deque->list (deque->list qoq))))
  ;computation
  (list->qoq (qoq->list the-qoq) ngroups))

(define (shuffle-bell lst)
  (bell)
  (shuffle lst))

(define compute-state 
  (gen:generator
   (names-pics)
   (let ((initial-state (list->qoq names-pics 1)))
     ;The LET is "next-state (loop current-state who num-groups)"
     (let loop ((current-state initial-state)
                (who 'initial)
                (num-groups 1))
       
       (define next-state
         (case who
           [(next)    (let ((g (pop-func-push (front current-state) shuffle-bell)))
                        (push-front (pop-front current-state) g))]
           [(previous) (let ((g (push-front (pop-back (front current-state)) 
                                            (back (front current-state)))))
                         (push-front (pop-front current-state) g))]
           
           [(omit)    (let ((g (pop-func (front current-state) shuffle-bell)))
                        (push-front (pop-front current-state) g))]
           
           [(group)   (pop-func-push  current-state identity)]
           [(previousgroup) 
            ;(display "previous group in compute state")
            (push-front (pop-back current-state) (back current-state))]
           
           [(menu)    (qoq->qoq current-state num-groups)]
           [(initial) current-state]
           [else (error "entered compute-state with unknown who")]))
       
       (define-values (next-who next-num-groups) (gen:yield next-state))
       
       (loop next-state 
             next-who 
             next-num-groups)))))
  