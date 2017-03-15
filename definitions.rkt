 #lang racket
(require "declarations.rkt")
(require "drawing-routine.rkt")
(require "testcases.rkt")
(provide buildTree calcForces moveparticles)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;Build Tree;;;;;;;;;;;;;;
(define (buildTree Area particles)
  (cond [(singleton particles)
         (let* [(p (car particles))
                (mass (particle-mass p))
                (posn (particle-posn p))]
           (gnode mass posn '()))]
        [(null? particles) '()]
        [else (let*[(xmin (bbox-llx Area))
                    (xmax (bbox-rux Area))
                    (ymin (bbox-lly Area))
                    (ymax (bbox-ruy Area))
                    (cx (/ (+ xmin xmax) 2))
                    (cy (/ (+ ymin ymax) 2))
                    (q1 (append* (filter (lambda(x) (let* [(p (particle-posn x))] (if (and (< (vec-x p) cx) (not (< (vec-y p) cy))) #t #f))) particles)))
                    (q2 (append* (filter (lambda(x) (let* [(p (particle-posn x))] (if (and (not (< (vec-x p) cx)) (not (< (vec-y p) cy))) #t #f))) particles)))
                    (q3 (append* (filter (lambda(x) (let* [(p (particle-posn x))] (if (and (< (vec-x p) cx) (< (vec-y p) cy)) #t #f))) particles)))
                    (q4 (append* (filter (lambda(x) (let* [(p (particle-posn x))] (if (and (not (< (vec-x p) cx)) (< (vec-y p) cy)) #t #f))) particles)))
                    (mass (sum (lc (particle-mass x) : x <- particles)))
                    (sumx (sum (lc (* (vec-x (particle-posn p)) (particle-mass p)) : p <- particles)))
                    (sumy (sum (lc (* (vec-y (particle-posn p)) (particle-mass p)) : p <- particles)))
                    (posn (vec (/ sumx mass) (/ sumy mass)))
                    (tq1 (buildTree (bbox xmin cy cx ymax) q1))
                    (tq2 (buildTree (bbox cx cy xmax ymax) q2))
                    (tq3 (buildTree (bbox xmin ymin cx cy) q3))
                    (tq4 (buildTree (bbox cx ymin xmax cy) q4))]                
                (gnode mass posn (remove-null (list tq4 tq3 tq2 tq1) '())))]))

;;;;;;;;;;;;;;;;;;;;calcForces;;;;;;;;;;;;;;;;;;;

(define (calcForces Area tree particles) 
  (helper (- (bbox-rux Area) (bbox-llx Area)) tree particles '()))

(define (helper s tree particles res)
  (cond [(null? particles) (reverse res)]
        [else (let* [(x (car particles))
                     (cparticles (close-particles x s tree '()))
                     (fx (lc (/ (* g (particle-mass x) (gnode-mass p) (- (vec-x (gnode-posn p)) (vec-x (particle-posn x)))) (expt (dist (particle-posn x) (gnode-posn p)) 3)) : p <- cparticles))
                     (fy (lc (/ (* g (particle-mass x) (gnode-mass p) (- (vec-y (gnode-posn p)) (vec-y (particle-posn x)))) (expt (dist (particle-posn x) (gnode-posn p)) 3)) : p <- cparticles))
                     (f (vec (sum fx) (sum fy)))]
                (helper s tree (cdr particles) (cons f res)))]))

(define (close-particles point s tree res)
  (cond [(null? tree) res]
        [(and (null? (gnode-subtrees tree)) (not (= 0 (dist (particle-posn point) (gnode-posn tree))))) (cons tree res)]
        [(< theta  (/ (dist (particle-posn point) (gnode-posn tree)) s)) (cons tree res)]
        [else (append (foldr (lambda (x y) (append (close-particles point (/ s 2) x res) y) ) '() (gnode-subtrees tree)) res)]))


;;;;;;;;;;;;;;;Move Particles;;;;;;;;;;;;;;;
(define (moveparticles particles forces)
  (define zparticles (zipwith vec particles forces))
  (define newx (lc (+ (vec-x (particle-posn (vec-x x))) (* timeslice (vec-x (particle-velocity (vec-x x)))) (* 0.5 timeslice timeslice (/ (vec-x (vec-y x)) (particle-mass (vec-x x)))) ) : x <- zparticles))
  (define newy (lc (+ (vec-y (particle-posn (vec-x x))) (* timeslice (vec-y (particle-velocity (vec-x x)))) (* 0.5 timeslice timeslice (/ (vec-y (vec-y x)) (particle-mass (vec-x x)))) ) : x <- zparticles))
  (define newvx (lc (+ (vec-x (particle-velocity (vec-x x))) (* timeslice (/ (vec-x (vec-y x)) (particle-mass (vec-x x))))): x <- zparticles))
  (define newvy (lc (+ (vec-y (particle-velocity (vec-x x))) (* timeslice (/ (vec-y (vec-y x)) (particle-mass (vec-x x))))): x <- zparticles))
  (define newpos (zipwith vec newx newy))
  (define newvel (zipwith vec newvx newvy))
  (define mass (lc (particle-mass x) : x <- particles))
  (zipwith3 particle mass newpos newvel))
  

;;;;;;;;;;Helper;;;;;;;;;;;;;;;;;
(define (filter p l)
  (cond [(null? l) '()]
        [(p (car l)) (cons (list (car l)) (filter p (cdr l)))]
        [else (filter p (cdr l))]))

(define (zipwith3 f l1 l2 l3)
  (cond [(or (null? l1) (null? l2)) '()] ;
        [else (cons (f (car l1) (car l2) (car l3)) (zipwith3 f (cdr l1) (cdr l2) (cdr l3)))]))

(define (dist p1 p2)
  (define square (lambda (x) (* x x)))
  (let*[(p1x (vec-x p1))
        (p1y (vec-y p1))
        (p2x (vec-x p2))
        (p2y (vec-y p2))
        (d (sqrt (+ (square (- p1x p2x)) (square (- p1y p2y)))))]
    d))

(define (remove-null l res)
  (if (null? l) res
      (if (null? (car l)) (remove-null (cdr l) res)
          (remove-null (cdr l) (cons (car l) res)))))







