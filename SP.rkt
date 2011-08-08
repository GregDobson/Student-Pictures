#lang racket
(require 2htdp/batch-io racket/gui/base)
(define names-filenames (read-csv-file "Class/Names.csv"))
;(define end-of-list-sound 
(define (read-picture triple) 
  (list 
   (first triple) 
   (second triple) 
   (read-bitmap (build-path "Class" (third triple)))))
(define names-pics (map read-picture names-filenames))  


(define (divide-into-short-lists the-list n)
  (if (<= (length the-list) n) 
      (list the-list)
      (cons (take the-list n) (divide-into-short-lists (drop the-list n) n))))
(define col-lists (divide-into-short-lists names-pics 3))






(define (make-frame col-lists)
  (define pics-window 
    (new frame% 
         (label " Students ") 
         (width 400)
         (height 300)
         (x 400)
         (y 100)))
  
  (define current-collection col-lists)
  (define (process-next-group b e) 
    (send  pics-window  delete-child the-panel)
    (set! current-collection (rest current-collection))
    (when (empty? current-collection) 
      (set! current-collection  col-lists))
    (set! the-panel (make-panel (first current-collection))))  
  (define top-panel 
    (new horizontal-panel% (parent pics-window))) 
  (define next-group-button  
    (new button% 
         (parent top-panel) 
         (label "Next Group")
         (callback process-next-group)))
  #;(define group-size-box 
    (new editor-canvas% 
         (parent top-panel)	 
 	 (editor (new text%))
         (style '(no-hscroll no-vscroll))))
  #;(send group-size-box allow-tab-exit #t)
  (define group-size-box
    (new text-field%
         (label #f)
         (parent top-panel)))
  ;(define set-group-button)
  
  (define (make-panel one-list)
    (define panel-shortcut%
      (class vertical-panel%
        (define/override (on-subwindow-char receiver event)
          (displayln  (send event get-key-code))
          (when (equal? (send event get-key-code) 'right) (send next-button command event))
          #t)
        (super-new)))
    (define (get-picture triple) (third triple))
    (define (get-name triple) (string-append (first triple) " " (second triple)))
    (define pics-panel (new panel-shortcut% (parent pics-window)))
    (define current-list one-list)
    (define picture (new message% (parent pics-panel)
                         (label (get-picture (first one-list)))))
    (define name (new message% 
                      (parent pics-panel) 
                      (label (get-name (first one-list))) 
                      (auto-resize #t)))  
    
    (define (process-next-person b e) 
      (set! current-list (rest current-list))
      (when (empty? current-list) 
        (set! current-list (shuffle one-list))
        (play-sound "/System/Library/Sounds/Submarine.aiff" #t))
      (send picture set-label (get-picture (first current-list)))
      (send name set-label (get-name (first current-list))))
    (define next-button (new button% (parent pics-panel) (label "Next Person")
                             (callback process-next-person)))
    pics-panel)
  
  (define the-panel (make-panel (first col-lists)))   
  
  (send pics-window show #t))

(make-frame col-lists)




