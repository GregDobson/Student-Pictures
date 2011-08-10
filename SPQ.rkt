#lang racket
(require data/queue 2htdp/batch-io racket/gui/base framework
         (prefix-in gen: racket/generator))

#|
(queue-length l-of-g)
|#

;SETUP: Read in data into big list
;create double queue structure from big list and nsets
;empty the double queue structure?
;display:  set bitmap and name of cperson on frame


;declarations
(struct person (name picture) #:transparent)
(struct state (cperson cgroup remaining) #:transparent #:mutable)
#;(define (list->queue list)
    (define queue (make-queue))
    (let loop ((templist list))
      (when (cons? templist)
        (enqueue! queue (first templist))
        (loop (rest templist))))
    queue) 

(define (list->queue list)
  (let ((queue (make-queue)))
    (for-each (lambda (x) (enqueue! queue x)) list)
    queue))

(define (lol->qoq lol)
  (list->queue (map list->queue lol)))

(define (qoq->lol qoq)
  (map queue->list (queue->list qoq)))

(define (qoq->state qoq)
  (let ((firstq (dequeue! qoq)))
    (state
     (dequeue! firstq)
     firstq
     qoq)))

(define (state->qoq st)
  (enqueue! (state-cgroup st) (state-cperson st))
  (enqueue! (state-remaining st) (state-cgroup st))
  (state-remaining st))

(define (next-person st)
  (displayln "-------- next person")
  (enqueue! (state-cgroup st) (state-cperson st))
  (set-state-cperson! st (dequeue! (state-cgroup st))))
(define (next-group st)
  (displayln "-------- next group")
  (enqueue! (state-cgroup st) (state-cperson st))
  (enqueue! (state-remaining st) (state-cgroup st))
  (set-state-cgroup! st (dequeue! (state-remaining st)))
  (set-state-cperson! st (dequeue! (state-cgroup st))))
                
(define (display-state st)
  (displayln (format "current person = ~a" (state-cperson st)))
  (displayln (format "current group = ~a" (queue->list (state-cgroup st))))
  (displayln (format "remaining groups = ~a" (qoq->lol (state-remaining st)))))





#|
(define test-lol (list (list 10 11 12) (list 20 21 22 23) (list 30 31 32 33 34)))
(define st (qoq->state (lol->qoq test-lol)))
(display-state st)
(next-person st)
(display-state st)
(next-group st)
(display-state st)
(next-person st)
(display-state st)
(next-group st)
(display-state st)
(next-person st)
(display-state st)
|#


;(qoq->lol (state->qoq (qoq->state (lol->qoq test-lol))))







;(queue->list (list->queue (list 1 2 3 4 5 6 7)))

#;(define (create-state list num-sets)
    (let (lol (divide-into-short-lists names-pics num-sets))))



#;(define qofg (make-queue))


;helper routines
(define (sound-shuffle lst)
  (bell)
  (shuffle lst))

(define (read-picture triple) 
  (person 
   (string-append (first triple) " " (second triple)) 
   (read-bitmap (build-path "Class" (third triple)))))


(define (make-iterator orig-lst func)
  (gen:generator ()
                 (let loop ((lst orig-lst))
                   (for-each gen:yield lst)
                   (loop (func orig-lst)))))


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

(define (pull-down-menu-pairs total )
  (let loop ((k 1))
    (let ((s (ceiling (/ total k))))
      (if (<= (/ total k) 5) 
          (list (list k s))
          (cons (list k s) (loop (ceiling (/ total (sub1 s)))))))))



(define (pull-down-menu-strings pairs)
  (define (one-string pair)
    (format "~a groups of size ~a" (first pair) (second pair)))
  (map one-string pairs))



;MAKE FRAME
(define (make-frame col-lists)
  (define actual-frame%
    (frame:standard-menus-mixin (frame:basic-mixin frame%)))
  
  ; helper routines
  (define (process-next-group b e) 
    (send  pics-window  delete-child the-panel)
    (define the-group (iter-groups))
    ;(displayln (format "current group =  ~a" (map person-name the-group)))
    (set! the-panel (make-panel the-group)))
  
  (define (num-sets-in-menu c)
    (first 
     (first 
      (drop menu-pairs (send c get-selection)))))
  
  (define (which-menu c e)
    (set! col-lists 
          (divide-into-short-lists names-pics (num-sets-in-menu c)))
    (set! iter-groups (make-iterator col-lists identity)) 
    ;(displayln (map person-name (first col-lists)))
    (send  pics-window  delete-child the-panel)
    (set! the-panel (make-panel (iter-groups))))
  
  ; MAKE PANEL
  (define (make-panel one-list)
    (define iter-people (make-iterator one-list sound-shuffle))
    (define (process-next-person b e) 
      (define one-person (iter-people))
      (send picture set-label (person-picture one-person))
      (send name set-label (person-name one-person)))
    
    ;setup the pane: picture, name, button for next person
    (define pics-panel (new vertical-panel% (parent pics-window)))
    (define picture (new message% (parent pics-panel)
                         (label (make-bitmap 300 400))))
    (define one-person (iter-people))
    (send picture set-label (person-picture one-person))
    
    (define name (new message% 
                      (parent pics-panel) 
                      (label (person-name one-person)) 
                      (auto-resize #t)))  
    (define next-button (new button% (parent pics-panel) (label "Next Person")
                             (callback process-next-person)))
    pics-panel)
  
  
  ;doing stuff
  (define pics-frame 
    (new actual-frame% 
         (label " Students ") 
         (width 400)
         (height 300)
         (x 400)
         (y 100)))
  (define pics-window (send pics-frame get-area-container))
  
  ;(displayln (format "starting frame #of collections= ~a" (length col-lists)))
  
  ; Setup the top panel on the frame:  next group button and pull down menu
  (define top-panel 
    (new horizontal-panel% (parent pics-window)))
  
  (define next-group-button  
    (new button% 
         (parent top-panel) 
         (label "Next Group")
         (callback process-next-group)))
  
  ; create the pull down menu for "how many groups of what size"
  (new choice% 
       (label #f)
       (choices menu-strings )
       (parent top-panel)
       (callback which-menu))
  
  
  (define the-panel (make-panel (iter-groups)))   
  (send pics-frame show #t)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;read in names, pictures, and create initial list-of-groups
(define names-filenames (read-csv-file "Class/Names.csv"))
(define names-pics (map read-picture names-filenames))  
(define the-state
  (qoq->state
   (lol->qoq (divide-into-short-lists names-pics 1))
(define iter-groups (make-iterator list-of-groups identity))

;create the pull down menu 
(define menu-pairs 
  (pull-down-menu-pairs
   (length names-pics)))
(define menu-strings
  (pull-down-menu-strings menu-pairs))

;do it
(make-frame list-of-groups)







