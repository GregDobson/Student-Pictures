#lang racket
(require 2htdp/batch-io racket/gui/base framework
         (prefix-in gen: racket/generator))
(require "SP-state-definition.rkt")
#|
(queue-length l-of-g)
|#

;SETUP: Read in data into big list
;create double queue structure from big list and nsets
;empty the double queue structure?
;display:  set bitmap and name of cperson on frame


;declarations
(struct person (name picture) #:transparent)



#;(define (qoq->lol qoq)
  (map queue->list (queue->list qoq)))


;helper routines
#;(define (sound-shuffle lst)
    (bell)
    (shuffle lst))

(define (read-picture triple) 
  (person 
   (string-append (first triple) " " (second triple)) 
   (read-bitmap (build-path "Class" (third triple)))))


#;(define (make-iterator orig-lst func)
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

; helper routines


;MAKE FRAME
(define (make-frame names-pics)
  (define actual-frame%
    (frame:standard-menus-mixin (frame:basic-mixin frame%)))
  
  ;helper routines
  (define the-state
     (lol->state (divide-into-short-lists names-pics 1)))
  
  
  (define menu-pairs 
    (pull-down-menu-pairs
     (length names-pics)))
  (define menu-strings
    (pull-down-menu-strings menu-pairs))
  
  (define (num-sets-in-menu menu)
    (first 
     (first 
      (drop menu-pairs (send menu get-selection)))))
  
  (define (display-state  st)
    (send picture set-label (person-picture (state-cperson st)))
    (send name set-label (person-name (state-cperson st))))
  
  (define (process-next-person b e) 
    (next-person the-state)
    (display-state the-state))
  
  (define (process-next-group b e)
    (next-group the-state)
    (display-state the-state))
  
  (define (which-menu c e)
    (set! the-state
          (lol->state
            (divide-into-short-lists 
             names-pics (num-sets-in-menu c)))))
  
  ;doing stuff
  (define pics-frame 
    (new actual-frame% 
         (label " Students ") 
         (width 400)
         (height 300)
         (x 400)
         (y 100)))
  
  (define pics-window (send pics-frame get-area-container))  
  
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
  
  
  ;setup the pane: picture, name, button for next person
  (define picture (new message% (parent pics-window)
                       (label (make-bitmap 300 400))))
  (define name (new message% 
                    (parent pics-window) 
                    (label "No Name") 
                    (auto-resize #t)))  
  (define next-button (new button% (parent pics-window) (label "Next Person")
                           (callback process-next-person)))
  ;Sets the picture
  (display-state the-state)
  
  
  
  
  (send pics-frame show #t)) 

(let ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;read in names, pictures, and create initial list-of-groups
  (define names-pics (map read-picture 
                          (read-csv-file "Class/Names.csv")))  
  
  ;do it
  (make-frame names-pics))







