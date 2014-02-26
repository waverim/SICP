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

; ex-2.12 make-center-percent
(define (make-center-percent c p)
  (make-interval (- c (abs (* c p)))
                 (+ c (abs (* c p)))))

(define (percent x)
  (let ((up (upper-bound x))
        (low (lower-bound x)))
    (/ (- up low)
       (+ up low))))

; ex-2.17 last element of a given (nonempty) list
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

; ex-2.18 reverse a list using iteration
(define (reverse-list l)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l)
              (cons (car l) result))))
  (iter l '()))

; ex-2.20 find same parity in a list using iteration
(define (same-parity x . y)
  (define (iter parity l result)
    (if (null? l) 
        result
        (if (parity (car l))
            (cons (car l) (iter parity (cdr l) result))
            (iter parity (cdr l) result))))
  (cons x (if (odd? x)
              (iter odd? y '())
              (iter even? y '()))))

; ex-2.22 square of a list
; first: define map function using iteration
(define (map-iter f l)
  (define (iter list result)
    (if (null? list)
        result
        (cons (f (car list))
              (iter (cdr list) result))))
  (iter l '()))
; to use square, do the following:
; (map-iter (lambda (x) (square x)) '(1 2 3 4 5)) 

; ex-2.27 deep reverse using iteration
(define (deep-reverse l)
  (define (iter list result)
    (if (null? list)
        result
        (if (not (pair? (car list)))
            (iter (cdr list) (cons (car list) result))
            (iter (cdr list) (cons (deep-reverse (car list)) result)))))
  (iter l (quote ())))

; ex-2.28 find leaves of a tree using iteration
(define (fringe t)
  (define (iter tree result)
    (if (null? tree)
        result
        (if (not (pair? tree))
            (cons tree result)
            (iter (car tree) 
                  (iter (cdr tree) result)))))
  (iter t (quote ())))

; ex-2.30 square a tree 
(define (square-tree tree)
  (cond ((null? tree) (quote ()))
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (x)
         (if (not (pair? x))
             (square x)
             (square-tree-map x)))
       tree))

; ex-2.31 improve tree using map
(define (tree-map f tree)
  (map (lambda (x)
         (cond ((null? x) (quote ()))
               ((not (pair? x)) (f x))
               (else (tree-map f x))))
       tree))

; ex-2.32 set of all subsets of the set
; from http://community.schemewiki.org by jz
; The set of all subsets of a given set is the union of:
; - the set of all subsets excluding the first number.
; - the set of all subsets excluding the first number, 
;   with the first number re-inserted into each subset.
(define (subsets s)
  (if (null? s)
      (list (quote ()))
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (x)
                       (cons (car s) x))
                     rest)))))