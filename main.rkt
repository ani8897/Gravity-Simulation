
#lang racket
(require "declarations.rkt")
(require "drawing-routine.rkt");;/// drawing module commented out
(require "testcases.rkt")
(require "definitions.rkt")

;-----
;;///Your implementation

;;///Use  the definition of (lc ...) in the class. This is given in
;;///the modified declarations.rkt that I am sending.
;;///Use the @ symbol before a guard.

;-----

;(define iter 2000);;/// iter is now defined in declarations.rkt

(define (main ps)
  (define (main-helper ps i)
    (cond [(> i iter) (display "Done")]
          [else (let*
                    ([ps-next (singlestep ps)])
                  (if (= (remainder iter drawtime) 0)
                      (begin 
                        (draw-particles ps) ;;/// Drawing routine
		                             ;;/// commented out
                        (main-helper ps-next (+ i 1)))
                      (main-helper ps-next (+ i 1))))]))
  (main-helper ps 0))

;;///Original singlestep
;(define (singlestep particles)
;  (let* ([initialArea (bounding-box particles)]
;         [tree (buildTree initialArea particles)] 
;         [forces (calcForces initialArea tree particles)]
;         [newparticles (moveparticles particles forces)])
;    newparticles))

;;///Modified singlestep -- TO BE USED FOR TESTING ONLY.

(define (singlestep particles)
  (let* ([initialArea (bounding-box particles)]
         [tree (buildTree initialArea particles)]  
         [forces (calcForces initialArea tree particles)]
         [newparticles (moveparticles particles forces)])
    (begin
      (display "-------------------------------------------")
      (newline)
      (newline)
      (display "Particles... Each particle is mass, position, velocity ") 
      (newline)
      (newline)
      (display-particles  particles)
      (newline)
      (newline)
      (display "Bounding box...")
      (newline)
      (newline)
      (display-bbox initialArea)
      (newline)
      (newline)
      (display "Tree...")
      (newline)
      (newline)
      (display-tree tree) 
      (newline)
      (newline)
      (display "Forces...")
      (newline)
      (newline)
      (display-forces forces)
      (newline)
      (newline)
      (display "Particles after single step.....")
      (newline)
      (newline)
      (display-particles newparticles)
      (newline)
      (newline)
      (display "End of iteration")
      (newline)
      (display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      newparticles)))                  

(main testList3)


