#lang racket
(require data/queue racket/gui/base)
(provide state-cperson state->lol next-person omit-person next-group create-state)
(struct state (cperson cgroup fgroup remaining) #:transparent #:mutable)

(define (divide-into-short-lists 
         the-list 
         num-lists)
  (define total (length the-list))
  (define chunk (floor (/ total num-lists)))
  (if (= total chunk) 
      (list the-list)
      (cons (take the-list chunk) 
            (divide-into-short-lists 
             (drop the-list chunk) 
             (sub1 num-lists)))))

(define (list->queue list)
  (let ((queue (make-queue)))
    (for-each (lambda (x) (enqueue! queue x)) list)
    queue))

(define (lol->state lol)
  (qoq->state (lol->qoq lol)))

(define (state->lol st)
  (qoq->lol (state->qoq st)))

(define (lol->qoq lol)
  (list->queue (map list->queue lol)))

(define (qoq->lol qoq)
  (map queue->list (queue->list qoq)))


(define (qoq->state qoq)
  (let ((firstq (dequeue! qoq)))
    (state
     (dequeue! firstq)
     firstq
     (make-queue)
     qoq)))

(define (create-state lst n)
  (lol->state (divide-into-short-lists lst n)))

(define (state->qoq st)
  (when (state-cperson st)
    (enqueue! (state-cgroup st) (state-cperson st)))
  (for-each 
   (lambda (x) (enqueue! (state-cgroup st) x)) 
   (queue->list (state-fgroup st)))
  (enqueue! (state-remaining st) (state-cgroup st))
  (state-remaining st))

(define (next-person st)
  (enqueue! (state-fgroup st) (state-cperson st))
  (goto-next-person st))

(define (omit-person st)
  (goto-next-person st))

(define (moveftoc st)
  (let ((t (queue->list (state-fgroup st))))
    (for-each 
     (lambda (x) (enqueue! (state-cgroup st) x))
     (shuffle t))
    (set-state-fgroup! st (make-queue))))

(define (goto-next-person st)
  (when (queue-empty? (state-cgroup st)) 
    (moveftoc st)
    (bell))
  (if (queue-empty? (state-cgroup st))
      (set-state-cperson! st #f)
      (set-state-cperson! st (dequeue! (state-cgroup st)))))



(define (next-group st)
  ;(display-state st)
  (enqueue! (state-cgroup st) (state-cperson st))
  (moveftoc st)
  (enqueue! (state-remaining st) (state-cgroup st))
  (set-state-cgroup! st (dequeue! (state-remaining st)))
  (set-state-cperson! st (dequeue! (state-cgroup st))))

(define (display-state st)
  (displayln (format "current person = ~a" (state-cperson st)))
  (displayln (format "current group = ~a" (queue->list (state-cgroup st))))
  (displayln (format "remaining groups = ~a" (qoq->lol (state-remaining st)))))

