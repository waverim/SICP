; description: my SICP exercise of charpet 2
; author: Waverim
; date: Feb 20, 2014

; function maybe useful
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (inc x) (+ x 1))
(define (average x y) (/ (+ x y) 2))

; ex-2.1 improve make-rat function to 
; handles both positive and negative arguments
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

; ex-2.2 make a line
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cadr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start-point end-point)
  (cons start-point (list end-point)))
(define (start-segment s) (car s))
(define (end-segment s) (cadr s))

(define (midpoint-segment s)
  (cons (average (x-point (start-segment s))
                 (x-point (end-segment s)))
        (list (average (y-point (start-segment s))
                       (y-point (end-segment s))))))

; ex-2.3 rectangle, use the top-left point, width and height
(define (rectangle width height start-point)
  (cons width (cons height (list start-point))))

(define (rectangle-circumference rectangle)
  (double (+ (car rectangle)
             (cadr rectangle))))

(define (rectangle-area rectangle)
  (* (car rectangle)
     (cadr rectangle)))

; construct (a b) of (exp 2 a)*(exp 3 b)
; and to the opposite to cons a and b
(define (cons-product a b)
  (* (expt 2 a)
     (expt 3 a)))

(define (find-exp n base)
  (define (iter try-exp)
    (if (= 0 (remainder n (expt base try-exp)))
        (iter (inc try-exp))
        (- try-exp 1)))
  (iter 1))

(define (car-product x) (find-exp x 2))
(define (cdr-product x) (find-exp x 3))

; ex-2.6 Church numerals
; zero = λf.λx.x
(define zero
  (lambda (f) 
    (lambda (x) x)))

; one = λf.λx.f x
(define one
  (lambda (f)
    (lambda (x) (f x))))

; two = λf.λx.f (f x)
(define two 
  (lambda (f)
    (lambda (x) (f (f x)))))

; plus = λm.λn.λf.λx.m f (n f x)
(define (church-plus m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

; Alyssa's problem: from ex-2.7 to ex-2.16 
; ex-2.7 make-interval
(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

; ex-2.8 sub-interval
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; ex-2.9 width
(define (width-interval x y)
  (halve (+ (- (upper-bound x) (lower-bound x))
            (- (upper-bound x) (lower-bound x)))))

; ex-2.10 improve div function
(define (div-interval x y)
  (if (or (= 0 (upper-bound y))
          (= 0 (lower-bound y)))
      (display "Error: Divide by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (lower-bound x) (lower-bound y)))
        (p4 (* (lower-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))