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

; ex-2.33 using accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-acc p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              (quote ())
              sequence))

(define (append-acc seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length-acc sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0 
              sequence))
                
; ex-2.34 Evaluating a polynomial 
; using Horner’s rule & accumulate
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

; ex-2.35 count-leaves using accumulate 
(define (count-leaves-acc t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (not (pair? x))
                         1
                         (count-leaves-acc x)))
                   t)))

; ex-2.36 accumulate-n
; from http://community.schemewiki.org by jz
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (quote ())
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; ex-2.37 martix using accumulate
(define (dot-product v w)
  (accumulate +
              0
              (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row)
         (dot-product m-row v))
       m))

; Define the matrix transpose, 
; makes the matrix multiplication easier
(define (transpose mat)
  (accumulate-n cons (quote ()) mat))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row)
           (matrix-*-vector n-cols m-row))
         m)))

; ex-2.38 function defined in the book
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; op must be commutative

; ex-2.39 reverse using fold-right & fold-left
(define (reverse-right sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              (quote ())
              sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y)
               (cons y x))
             (quote ())
             sequence))

; ex-2.40 using unique-pairs to redefine prime-pair-sum

; "is a prime" function from charpet 1 in the book
(define (prime? n)
  (define (smallest-divisor n) (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

  (define (divides? a b) (= (remainder b a) 0))

  (= n (smallest-divisor n)))

; functions in the book
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair)))) 

(define (flatmap proc seq)
  (accumulate append (quote ()) (map proc seq)))

; functions should be defined
(define (enumerate-interval low high)
  (if (> low high)
      (quote ())
      (cons low (enumerate-interval (inc low) high))))

(define (filter f s)
  (cond ((null? s) (quote ()))
        ((f (car s))
         (cons (car s)
               (filter f (cdr s))))
        (else (filter f (cdr s)))))

; unique-pairs
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; prime-sum-pairs using unique-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; ex-2.41 define prime-sum-triples 
; using unique-triples & functions in the ex-2.40

; sum a list
(define (list-sum l)
  (if (null? l)
      0
      (+ (car l) (list-sum (cdr l)))))

; Determine whether the sum is a prime
(define (prime-list-sum? list)
  (prime? (list-sum list)))

; make a list with sum, rebuild the list 
; it may be a little repetitive
(define (make-list-sum lat)
  (define (iter l result)
    (if (null? l)
        result
        (cons (car l) (iter (cdr l) result))))
  (iter lat (list (list-sum lat))))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) 
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-triples n)
  (map make-list-sum
       (filter prime-list-sum? (unique-triples n))))

; ex-2.42 eight-queens puzzle
; Reference from huangz1990's blog 
; http://sicp.readthedocs.org/
(define empty-board (quote ()))

(define (adjoin-position new-row board-size rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? board-size position)
  (define (iter row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
          (if (or (= row-of-new-queen row-of-current-queen)
                  (= row-of-new-queen (+ row-of-current-queen i))
                  (= row-of-new-queen (- row-of-current-queen i)))
              #f
              (iter row-of-new-queen
                    (cdr rest-of-queens)
                    (+ i 1))))))
  (iter (car position) (cdr position) 1))

(define (queen board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; one example in the book

; 1                o
; 2       o
; 3 o
; 4                   o
; 5             o
; 6                      o
; 7    o
; 8          o
;   1  2  3  4  5  6  7  8

; ex-2.54 equal? using recursion
(define (equal-rec? l1 l2)
  (cond ((and (not (pair? l1)) (not (pair? l2)))
         (eq? l1 l2))
        ((and (pair? l1) (pair? l2))
         (and (equal-rec? (car l1) (car l2))
              (equal-rec? (cdr l1) (cdr l2))))
        (else #f)))

; ex-2.56 exponentiation
; code in the book
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
;(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; exponentiation
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base exp)
  (cond ((=number? base 1) 1)
        ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplier exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product 
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (if (number? (exponent exp))
                                                 (- (exponent exp) 1)
                                                 (' (- (exponent exp) 1)))))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

; ex-2.57 sum or mulitiply a list
(define (augend s)
  (accumulate make-sum 0 (cddr s)))

(define (mutiplicand p)
  (accumulate make-product 1 (cddr p)))

; ex-2.59 union-set
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; conventional methods
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (cons (car s1)
                    (union-set (cdr s1) s2)))))

; using filter
(define (union-set-filter s1 s2)
  (append s1
          (filter (lambda (x)
                    (not (element-of-set? x s1)))
                  s2)))

; ex-2.61 adjoin-set in sorted list
(define (adjoin-set-sorted x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set-sorted x (cdr set))))))

; ex-2.62 union-set in sorted list
(define (union-set-sorted s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((x1 (car s1))
                    (x2 (car s2)))                        
                (cond ((= x1 x2)
                       (cons x1 (union-set-sorted (cdr s1) (cdr s2))))
                      ((< x1 x2)
                       (cons x1 (union-set-sorted (cdr s1) s2)))
                      (else 
                       (cons x2 (union-set-sorted s1 (cdr s2)))))))))

; [Book] tree
; basic function of operating a tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-tree? x tree)
  (cond ((null? tree) #f)
        ((= x (entry tree)) true)
        ((< x (entry tree)) 
         (element-of-tree? x (left-branch tree)))
        (else 
         (element-of-tree? x (right-branch tree)))))

(define (adjoin-tree x tree)
  (cond ((null? tree) (make-tree x (quote ()) (quote ())))
        ((= x (entry tree)) tree)
        ((< x (entry tree))
         (make-tree (entry tree)
                    (adjoin-tree x (left-branch tree))
                    (right-branch tree)))
        (else
         (make-tree (entry tree)
                    (left-branch tree)
                    (adjoin-tree x (right-branch tree))))))

; tree->list
(define (tree->list-1 tree)
  (if (null? tree) 
      (quote ())
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (defien (copy-to-list tree result-list)
    (if (null? tree)
        (quote ())
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree (quote ())))

; Huffman
; code in the book
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; decode
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        (quote ())
        (let ((next-branch 
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set) (cons x set))))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      (quote ())
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; ex-2.67 sample code in the book
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1  1 1 0))

; answer: (a d a b b c a)

; ex-2.68 encode
(define (encode message tree)
  (if (null? message)
      (quote ())
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; encode-symbol function
; from http://community.schemewiki.org/ by siki
(define (encode-symbol symbol tree)
  (cond ((not (element-of-set? symbol (symbols tree))) #f)
        ((leaf? tree) (quote ()))
        (else (let ((left-set (symbols (left-branch tree)))
                    (right-set (symbols (right-branch tree))))
                (cond ((element-of-set? symbol left-set)
                       (cons 0 (encode-symbol symbol (left-branch tree))))
                      ((element-of-set? symbol right-set)
                       (cons 1 (encode-symbol symbol (right-branch tree)))))))))

; ex-2.69 generate huffman tree
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
       (adjoin-set 
        (make-code-tree (car leaf-set)
                        (cadr leaf-set))
        (caddr leaf-set)))))

; ex-2.75 message passing
(define (make-from-mag-ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else #f)))
  dispatch)

; put & get
; it's not given in the book, but may be useful
; http://stackoverflow.com/a/5499256/3289554
(define global-array '())
(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array)
                      (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

; ex-2.78 redefine function using number?
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else #f)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else #f)))

; ex-2.79 equ?
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

(define (install-rational-package)
  (define (equ? x y)
    (= (* (number x) (denom y))
       (* (number y) (denom y))))
  (put 'equ? '(rational rational) equ?)
  'done)

(define (install-complex-package)
  (define (equ? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (put 'equ? '(complex complex) equ?)
  'done)

; ex-2.80
(define (=zero? x) (apply-generic '=zero? x))

(define (install-zero-package)
  (put '=zero? 'scheme-number (lambda (x) (= x 0)))
  (put '=zero? 'rational-number (lambda (x) (= (number x) 0)))
  (put '=zero? 'complex-number 
       (lambda (x) (= (real-part x) (imag-part x) 0))))

; ex-2.81 redefine apply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-args))
                    ((a1 (car args)))
                    ((a2 (cadr args))))
                (if (equal? type1 type2)
                    (list op type-tags)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (list op type-tags))))))
              (list op type-tags))))))