#lang racket
(require (prefix-in srfi: srfi/48))
(provide (struct-out particle) (struct-out gnode) (struct-out bbox) (struct-out vec)
         theta iter g timeslice drawtime zipwith singleton sum concat lc bounding-box
         display-forces display-particles display-bbox display-tree)

(define format-string "~4,3F") ;; This is what decides the output format of numbers

(define (print-formatted num)  ;; This is what prints floating point numbers 
  (display  (srfi:format format-string num)))


; A particle is a struct with  a record of its mass, its location, and
; its velocity:

(struct particle (mass posn velocity) #:transparent)

;The struct representing the tree

(struct gnode (mass posn subtrees) #:transparent)

;Struct for the bounding box. Specifies the box by giving lower left x
;and y axis and upper right x and y axis values.

(struct bbox   (llx lly rux ruy) #:transparent)

; struct representing the vector of a particle in 2D space.

(struct vec (x y) #:transparent)

;Global declarations

(define theta 2)     ;decides which particles are deemed close to one another
(define iter 10000)    ;decides number of iterations to run
(define g 18)    ;gravitational constant in UniverseX
(define timeslice .01) ;the simulation is updated after unit time
(define drawtime 1)    ;Draw every drawtime iterations

(define (zipwith f l1 l2)
  (cond [(or (null? l1) (null? l2)) '()] ;
        [else (cons (f (car l1) (car l2)) (zipwith f (cdr l1) (cdr l2)))]))

(define (singleton l) ( = (length l) 1))

(define (sum l) (foldl + 0 l))
(define (concat l) (foldr append `() l))

(define (bounding-box particles)
  (define (fn-upper nextp acc) (max (vec-x nextp) (vec-y nextp) acc))
  (define (fn-lower nextp acc) (min (vec-x nextp) (vec-y nextp) acc))
  (let ([ur (+ 1 (foldr fn-upper -50000000 (map particle-posn particles)))]
        [ll (- (foldr fn-lower  50000000 (map particle-posn particles)) 1)])
    (bbox ll ll ur ur )))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))


(define (display-vec t)
  (display "(")
  (print-formatted (vec-x t))
  (display ", ")
  (print-formatted (vec-y t))
  (display ")"))

(define (display-particle p)
  (begin
    (display "(particle: ")
    (print-formatted (particle-mass p))
    (display ", ")
    (display-vec (particle-posn p))
    (display ", ")
    (display-vec (particle-velocity p))
    (display ")")))

(define (display-particles ps)
  (cond [(not (null? ps))
         (begin
           (display "[")
           (display-particles-helper ps 1)
           (display "]"))]))

(define (display-particles-helper ps indent)
  (cond [(not (null? ps)) (begin
                            (printblanks indent)
                            (display-particle (car ps))
                            (newline)
                            (display-particles-helper (cdr ps) 2)
                            )]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-force p)
  (begin
    (display "(force: ")
    (display-vec p)
    (display ")")))

(define (display-forces ps)
  (cond [(not (null? ps))
         (begin
           (display "[")
           (display-forces-helper ps 1)
           (display "]"))]))

(define (display-forces-helper ps indent)
  (cond [(not (null? ps)) (begin
                            (printblanks indent)
                            (display-force (car ps))
                            (newline)
                            (display-forces-helper (cdr ps) 2)
                            )]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Each entity is placed at the begining of a line and given a certain
; indent and at the end of printing leaves the state at the begining
; of a line
(define (display-tree t)
  (display-tree-helper t 0))

(define (display-tree-helper t indent)
  (cond [(null? (gnode-subtrees t))
         (begin
           (display "(leaf ")
           (print-formatted (gnode-mass t))
           (display " ")
           (display-vec (gnode-posn t))
           (display ")"))]
        [else (display "(gnode ")
              (print-formatted (gnode-mass t))
              (display " ")
              (display-vec (gnode-posn t))
              (newline)
              (printblanks (+ indent 2))
              (display "[")
              (display-list (gnode-subtrees t) (+ indent 3))
              (display "])")
              
              ]))
;
(define (display-list lt indent)
  (begin
    (cond [(not (null? lt))
           (begin                      
             (display-tree-helper (car lt) indent)
             (cond [(not (null? (cdr lt)))
                    (begin
                      (newline)
                      (printblanks indent)
                      (display-list (cdr lt) indent))]))])))

(define (display-bbox bb)
  (begin
    (display "(bbox: ")
    (print-formatted (bbox-llx bb))
    (display " ")
    (print-formatted (bbox-lly bb))
    (display " ")
    (print-formatted (bbox-rux bb))
    (display " ")
    (print-formatted (bbox-ruy bb))
    (display ")")
    (newline)))
                     
(define (printblanks indent)
  (cond[(not (= indent 0)) 
      (begin
        (display " ") 
        (printblanks (- indent 1)))]))



;(define indent 0)

;(define (display-tree t)
;  (display-tree-helper t))
;
;(define (display-tree-helper t)
;  (cond [(null? (gnode-subtrees t))
;         (begin
;           (display "(leaf ")
;           (print-formatted (gnode-mass t))
;           (display " ")
;           (display-vec (gnode-posn t))
;           (display ")"))]
;        [else (display "(gnode ")
;              (print-formatted (gnode-mass t))
;              (display " ")
;              (display-vec (gnode-posn t))
;              (newline)
;              (printblanks (+ indent 2))
;              (display "[")
;              (set! indent (+ indent 3))
;              (display-list (gnode-subtrees t))
;              (set! indent (- indent 3))
;              (display "])")
;              
;              ]))

;(define (display-list lt)
;  (begin
;    (cond [(not (null? lt))
;           (begin ;(printblanks indent)
;             (display-tree-helper (car lt))
;             (cond [(not (null? (cdr lt)))
;                    (begin
;                      (newline)
;                      (printblanks indent)
;                      (display-list (cdr lt)))]))])))