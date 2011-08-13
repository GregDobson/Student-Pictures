#lang racket
(require 2htdp/batch-io racket/gui/base framework
         (prefix-in gen: racket/generator))
(require "SP-state-definition.rkt")

;declarations
(struct person (name picture) #:transparent)



;helper routines
#;(define (sound-shuffle lst)
    (bell)
    (shuffle lst))

(define (read-picture base-of-path triple) 
  (person 
   (string-append (first triple) " " (second triple)) 
   (read-bitmap (build-path base-of-path (third triple)))))


#;(define (make-iterator orig-lst func)
    (gen:generator ()
                   (let loop ((lst orig-lst))
                     (for-each gen:yield lst)
                     (loop (func orig-lst)))))



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
  (define the-state  (create-state names-pics 1))
  
  
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
    (if (state-cperson st)
        (begin
          (send picture set-label (person-picture (state-cperson st)))
          (send name set-label (person-name (state-cperson st))) )
        (begin
          (send picture set-label (make-bitmap 300 400))
          (send name set-label "no more people in group"))))
  
  (define (process-next-person b e) 
    (next-person the-state)
    (display-state the-state))
  
  (define (process-omit-person b e)
    (omit-person the-state)
    (display-state the-state))
  
  (define (process-next-group b e)
    (next-group the-state)
    (display-state the-state))
  
  (define (which-menu menu e)
    (set! the-state (create-state 
                     (append* (state->lol the-state))
                     (num-sets-in-menu menu)))
    (display-state the-state))
  
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
  (define bot-panel 
    (new horizontal-panel% (parent pics-window)))
  (define next-button (new button% (parent bot-panel) (label "Next Person")
                           (callback process-next-person)))
  (define omit-button (new button% (parent bot-panel) (label "Omit Person")
                           (callback process-omit-person)))
  
  ;Sets the picture
  (display-state the-state)
  
  
  
  
  (send pics-frame show #t)) 



;(displayln (current-command-line-arguments))
(let ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;read in names, pictures, and create initial list-of-groups
  ;(application-file-handler file-handler)
  
  (define filepath 
    (if (= 0 (vector-length (current-command-line-arguments)))
        (finder:get-file)
        (vector-ref (current-command-line-arguments) 0)))
  
  (when filepath  
    (define-values (base-of-path _1 _2) (split-path filepath)) 
    (define names-pics      
      (read-csv-file/rows 
       (path->string filepath) 
       (lambda (x) (read-picture base-of-path x))))  
    (make-frame names-pics)))







