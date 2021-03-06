#lang racket
(require 2htdp/batch-io racket/gui/base framework
         (prefix-in gen: racket/generator )
         "deque.rkt"
         "SPN-state.rkt")


;declarations...
(struct person (name picture) #:transparent)

;routine reads data from appropriate place, returns list of struct person
(define (read-data)
  (define (read-picture base-of-path triple)
    (displayln (format "first= ~a  second = ~a  filename= ~a   baseofpath= ~a"
                       (first triple) 
                       (second triple) (third triple)
                       (path->string base-of-path)))
    (person 
     (string-append (first triple) " " (second triple))
     (let ((path (build-path base-of-path (third triple))))
       (if (file-exists? path)
           (let* ((mybitmap (make-bitmap 300 400) )
                 (abitdc (new bitmap-dc% (bitmap mybitmap)))
                  (read-bitmap (read-bitmap path) ))
             (send abitdc draw-bitmap-section-smooth read-bitmap
                   0 0 300 400 0 0 (send read-bitmap get-width) (send read-bitmap get-height))
            mybitmap)
           (make-bitmap 300 400)))))
  
  (define filepath
    #;(string->path "Class/Names.csv")
    (if (= 0 (vector-length (current-command-line-arguments)))
        (finder:get-file)
        (vector-ref (current-command-line-arguments) 0)))
  
  (when filepath  
    (define-values (base-of-path _1 _2) (split-path filepath))
    (read-csv-file/rows 
     (path->string filepath) 
     (lambda (x) (read-picture base-of-path x)))))


;GUI structure
(define (GUI)
  ;sets/resets the counts on the window for the group and the person
  (define (display-counts group person counts)
    (send group set-label 
          (format "~a of ~a" (add1 (counts-i-group counts))
                  (counts-t-groups counts)))
    (send person set-label 
          (format "~a of ~a" (add1 (counts-j-person counts))
                  (counts-t-persons counts))))
  
  ;sets/resets the picture and name of the current picture in the window
  (define (display-person  st)
    (let ((person (front (front (state-qoq st)))))
      (if person
          (begin
            (send picture set-label (person-picture person))
            (send name set-label (person-name person)))
          (begin
            (send picture set-label (make-bitmap 300 400))
            (send name set-label "no more people in group"))))
    (display-counts which-group which-person (state-cnts st)))
  
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
        (format "~a group~a of size ~a" 
                (first pair) (if (= (first pair) 1) "" "s") (second pair)))
      (map one-string pairs))
    
    ;computation
    (define menu-pairs (pull-down-menu-pairs (count-qoq (state-qoq the-state))))
    (define menu-strings (pull-down-menu-strings menu-pairs))
    (send the-menu clear)
    (send the-menu append "               ")
    (map (lambda (item) (send the-menu append item)) menu-strings))
  
  ;routines that respond to buttons and menus
  (define (process-next-person b e) 
    (display-person (compute-state 'next 0)))
  
  (define (process-previous-person b e)
    (display-person (compute-state 'previous 0)))
  
  (define (process-omit-person b e)
    (let ((the-state (compute-state 'omit 0)))
      (display-person the-state)
      (display-menu the-menu the-state)))
  
  (define (process-next-group b e)
    (display-person (compute-state 'group 0)))
  
  (define (process-previous-group b e)
    (display-person (compute-state 'previousgroup 0)))
  
  
  
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
         (alignment  '(center center))
         (width 400)
         (height 300)
         (x 400)
         (y 100)))
  
  (define pics-window (send pics-frame get-area-container))  
  
  ; Setup the front panel on the frame:  next group button and pull down menu
  (define top-panel 
    (new horizontal-panel% (parent pics-window)
         (alignment '(center center)) ))
  
  (define the-menu 
    (new choice% 
         (label #f)
         (choices (list "-------------") )
         (parent top-panel)
         (callback which-menu)))
  
  (define group-panel 
    (new horizontal-panel% (parent pics-window)
         (alignment '(center center))))
  
  
  ; create the pull down menu for "how many groups of what size"
  (define previous-group-button  
    (new button% 
         (parent group-panel) 
         (label "Previous Group")
         (callback process-previous-group)
         (min-width 20)))
  
  (define which-group (new message% 
                           (parent group-panel) 
                           (label "100 of 100") 
                           (auto-resize #f)))
  
  (define next-group-button  
    (new button% 
         (parent group-panel) 
         (label "Next Group")
         (callback process-next-group)
         (min-width 20)))
  
  
  
  ;setup the pane: picture, name, 
  (define pics-name-panel 
    (new vertical-panel% (parent pics-window)
         (alignment '(center center))))
  
  (define picture (new message% (parent pics-name-panel)
                       (label (make-bitmap 300 400))))
  (define name (new message% 
                    (parent pics-name-panel) 
                    (label "No Name") 
                    (auto-resize #t)
                    ;(stretchable-width #t)
                    ))
  
 ; setup the pane for  buttons for next person 
  (define person-panel 
    (new horizontal-panel% (parent pics-window)
         (alignment '(center center) )))
  
  
  (define previous-button (new button% (parent person-panel) (label "Previous Person")
                               (callback process-previous-person)))
  
  (define which-person (new message% 
                            (parent person-panel) 
                            (label "12 of 100") 
                            (auto-resize #f)))
  
  (define next-button (new button% (parent person-panel) (label "Next Person")
                           (callback process-next-person)))
  
  ;setup button for omit person
  (define omit-panel 
    (new horizontal-panel% (parent pics-window)
         (alignment '(center center) )))
  
  (define omit-button (new button% (parent omit-panel) (label "Omit Person")
                           (callback process-omit-person)))
  
  ;make frame visible, read data and compute initial state
  (define the-state (compute-state (read-data)))
  
  ;display initial state
  (display-person the-state)
  (display-menu the-menu the-state)
  (send pics-frame show #t)
  
  #f)

;do it
(GUI)

