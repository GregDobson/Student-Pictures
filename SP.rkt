#lang racket
(require 2htdp/batch-io racket/gui/base framework)

;declarations
(struct person (name picture) #:transparent)


;helper routines
(define (sound-shuffle lst)
  (bell)
  (shuffle lst))

(define (read-picture triple) 
  (person 
   (string-append (first triple) " " (second triple)) 
   (read-bitmap (build-path "Class" (third triple)))))

(define (make-iterator lst func)
  (define remaining lst)
  (lambda ()
    (define result (first remaining))
    (set! remaining (rest remaining))
    (when (empty? remaining)
      (set! remaining (func lst)))
    result))

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
  (define (pdml k)
    (define s (ceiling (/ total k)))
    (if (<= (/ total k) 5) 
        (list (list k s))
        (cons (list k s) (pdml (ceiling (/ total (sub1 s)))))))
  (pdml 1))

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
  
  (define (which-menu c e)
    (define which (send c get-selection))
    ;(displayln which)
    (define nsets 
      (first 
       (first 
        (drop menu-pairs which))))
    ;(displayln (format " at which menu nsets= ~a" nsets))
    (set! col-lists 
          (divide-into-short-lists names-pics nsets))
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
  (define list-of-groups (divide-into-short-lists names-pics 1))
  (define iter-groups (make-iterator list-of-groups identity))
  
  ;create the pull down menu 
  (define menu-pairs 
    (pull-down-menu-pairs
     (length names-pics)))
  (define menu-strings
    (pull-down-menu-strings menu-pairs))
  
  ;do it
  (make-frame list-of-groups)
  
  
  
  
  