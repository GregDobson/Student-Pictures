#lang racket


;  read data
;  setup GUI structure
;  state ( compute-state initial)
;  finish GUI based on state
;  done (return control to GUI)

;  each button or menu x has routine
;    state (compute-state x)
;    finish GUI based on state
;    done(return control to GUI)


(define compute-state (generator (initial-who))
  ;initial-who is "initial" here so compute initial-state
  
  ;The LET is "next-state (loop current-state who)"
  
  (let loop ((current-state initial-state) (who initial-who))
    (define next-state ( ;...based on case...
                        
                        
                        (case who
                          [#; next-person ]
                          [#; omit-person]
                          [#; next-group]
                          [#; menu-picked])))
    
    
    
    (define next-who (yield next-state))
    (loop next-state next-who)))

