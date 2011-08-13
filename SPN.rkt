#lang racket
(require 2htdp/batch-io racket/gui/base framework
         (prefix-in gen: racket/generator )
         "queue.rkt"
         "SPN-state.rkt")

;declarations
(struct person (name picture) #:transparent)

;routine reads data from appropriate place, returns list of struct person
(define (read-data)
  (define (read-picture base-of-path triple) 
    (person 
     (string-append (first triple) " " (second triple)) 
     (read-bitmap (build-path base-of-path (third triple)))))
  
  (define filepath
    (string->path "Class/Names.csv")
    #;(if (= 0 (vector-length (current-command-line-arguments)))
          (finder:get-file)
          (vector-ref (current-command-line-arguments) 0)))
  
  (when filepath  
    (define-values (base-of-path _1 _2) (split-path filepath))
    (read-csv-file/rows 
     (path->string filepath) 
     (lambda (x) (read-picture base-of-path x)))))




;GUI structure
(define (GUI)
  ;sets/resets the picture and name of the current picture in the window
  (define (display-person  st)
    (let ((person (top (top st))))
      (if person
          (begin
            (send picture set-label (person-picture person))
            (send name set-label (person-name person)))
          (begin
            (send picture set-label (make-bitmap 300 400))
            (send name set-label "no more people in group")))))
  
  ;sets/resets the menu with the correct options
  (define (display-menu the-menu the-state)
    ;helpers
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
    
    ;computation
    (define menu-pairs (pull-down-menu-pairs (count-qoq the-state)))
    (define menu-strings (pull-down-menu-strings menu-pairs))
    (send the-menu clear)
    (send the-menu append "               ")
    (map (lambda (item) (send the-menu append item)) menu-strings))
  
  ;routines that respond to buttons and menus
  (define (process-next-person b e) 
    (display-person (compute-state 'next 0)))
  
  (define (process-omit-person b e)
    (let ((the-state (compute-state 'omit 0)))
      (display-person the-state)
      (display-menu the-menu the-state)))
  
  (define (process-next-group b e)
    (display-person (compute-state 'group 0)))
  
  (define (which-menu the-menu e)
    (let ((selection   (send the-menu get-selection)))
      (unless (zero? selection)
        (display-person (compute-state 'menu selection)))))
  
  ;sets up the standard menus, so, for example, cmd-W works 
  (define actual-frame%
    (frame:standard-menus-mixin (frame:basic-mixin frame%)))
  
  
  ;define the frame
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
  
  ; create the pull down menu for "how many groups of what size"
  (define the-menu 
    (new choice% 
         (label #f)
         (choices (list "---------------------") )
         (parent top-panel)
         (callback which-menu)))
  
  (define next-group-button  
    (new button% 
         (parent top-panel) 
         (label "Next Group")
         (callback process-next-group)))
  
  
  
  ;setup the pane: picture, name, buttons for next person and omit person
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
 
  ;make frame visible, read data and compute initial state
  (send pics-frame show #t)
  (define the-state (compute-state (read-data)))
  
  ;display initial state
  (display-person the-state)
  (display-menu the-menu the-state)
  #f)

;do it
(GUI)

