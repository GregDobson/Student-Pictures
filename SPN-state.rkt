#lang racket
(require 2htdp/batch-io racket/gui/base framework
         (prefix-in gen: racket/generator) "deque.rkt")
(provide compute-state 
         counts-i-group counts-t-groups counts-j-person counts-t-persons
         state-qoq state-cnts)
(struct counts (i-group t-groups j-person t-persons) #:transparent)
(struct state (qoq cnts))

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
   (let* ((initial-qoq (list->qoq names-pics 1) )
          (initial-counts (counts 0  (count initial-qoq) 
                                  0 (count (front initial-qoq))))
          (initial-state (state initial-qoq initial-counts)))
     ;The LET is "next-state (loop current-state who num-groups)"
     (let loop ((who 'initial)
                (current-qoq initial-qoq) 
                (current-counts initial-counts)
                (num-groups 1))
       
       
       
       (define next-qoq
         (case who
           [(next)    (let ((g (pop-func-push (front current-qoq) shuffle-bell)))
                        (push-front (pop-front current-qoq) g))]
           
           [(previous) (let ((g0 (front current-qoq)))
                         (if (empty-deque? g0)
                             g0
                             (let ((g (push-front (pop-back g0) (back g0))))
                               (push-front (pop-front current-qoq) g))))]
           
           [(omit)    (let ((g (pop-func (front current-qoq) shuffle-bell)))
                        (if (empty-deque? g)
                            (pop-front current-qoq)
                            (push-front (pop-front current-qoq) g)))]
           
           [(group)   (push-back (pop-front current-qoq) (front current-qoq))]
           
           [(previousgroup) (push-front (pop-back current-qoq) (back current-qoq))]
           
           [(menu)    (qoq->qoq current-qoq num-groups)]
           
           [(initial) current-qoq]
           
           [else (error "entered compute-state with unknown who")]))
       
       (define next-counts 
         (case who 
           [(next)       (let ((gs (counts-t-persons current-counts)))
                           (counts (counts-i-group current-counts)
                                   (counts-t-groups current-counts)
                                   (modulo (add1 (counts-j-person current-counts)) gs)
                                   gs)) ]
           
           [(previous)   (let ((gs (counts-t-persons current-counts)))
                           (counts (counts-i-group current-counts)
                                   (counts-t-groups current-counts)
                                   (modulo (sub1 (counts-j-person current-counts)) gs) 
                                   gs)) ]
           
           [(omit)        (let ((gs (sub1 (counts-t-persons current-counts))))
                            (if (> gs 0)
                                (counts (counts-i-group current-counts)
                                        (counts-t-groups current-counts)
                                        (modulo (counts-j-person current-counts) gs)
                                        gs)
                                (let ((ng (count next-qoq)))
                                  (if (> ng 0)
                                      (counts 0 (count  next-qoq) 
                                              0 (count (front next-qoq)))
                                      (exit)))))]
           
           [(group)       (let ((gs (count (front next-qoq))))
                            (counts (modulo (add1 (counts-i-group current-counts))
                                            (counts-t-groups current-counts))
                                    (counts-t-groups current-counts) 
                                    0 
                                    gs)) ]
           
           [(previousgroup) (let ((gs (count (front next-qoq))))
                              (counts (modulo (sub1 (counts-i-group current-counts))
                                              (counts-t-groups current-counts))
                                      (count  next-qoq) 
                                      0
                                      gs)) ]
           
           [(menu)        (counts 0 (count  next-qoq) 
                                  0 (count (front next-qoq))) ]
           [(initial) current-counts ]
           
           [else (error "entered compute-state with unknown who")]))
       
       (define next-state (state next-qoq next-counts))
       (define-values (next-who next-num-groups) (gen:yield next-state))
       
       (loop next-who 
             next-qoq 
             next-counts
             next-num-groups))))) 
