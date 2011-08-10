#lang racket
(require data/queue)
(provide state-cperson lol->state next-person next-group)
(struct state (cperson cgroup remaining) #:transparent #:mutable)

(define (list->queue list)
  (let ((queue (make-queue)))
    (for-each (lambda (x) (enqueue! queue x)) list)
    queue))

(define (lol->state lol)
  (qoq->state (lol->qoq lol)))

(define (lol->qoq lol)
  (list->queue (map list->queue lol)))

(define (qoq->state qoq)
  (let ((firstq (dequeue! qoq)))
    (state
     (dequeue! firstq)
     firstq
     qoq)))

#;(define (state->qoq st)
    (enqueue! (state-cgroup st) (state-cperson st))
    (enqueue! (state-remaining st) (state-cgroup st))
    (state-remaining st))

(define (next-person st)
  (enqueue! (state-cgroup st) (state-cperson st))
  (set-state-cperson! st (dequeue! (state-cgroup st))))

(define (next-group st)
  (enqueue! (state-cgroup st) (state-cperson st))
  (enqueue! (state-remaining st) (state-cgroup st))
  (set-state-cgroup! st (dequeue! (state-remaining st)))
  (set-state-cperson! st (dequeue! (state-cgroup st))))

#;(define (display-state st)
    (displayln (format "current person = ~a" (state-cperson st)))
    (displayln (format "current group = ~a" (queue->list (state-cgroup st))))
    (displayln (format "remaining groups = ~a" (qoq->lol (state-remaining st)))))

