; description: my SICP exercise of charpet 1
; author: Waverim
; date: Feb 16, 2014

; function maybe useful
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

; ex-1.3 sum of larger two
(define sum-of-larger-two
  (lambda (x y z)
    (if (> x y)
        (if (> y z) 
            (+ x y)
            (+ x z))
        (if (> x z)
            (+ y x)
            (+ y z)))))

; ex-1.8 cube roots by Newton's method using block structure
(define (cube-root x) 
  (define (cube-root-iter guess)
    (if (good-enough? guess)
      guess
      (cube-root-iter (improve guess))))

  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  
  (cube-root-iter 1.0))

; ex-1.11 
; iter function is a reference from internet
(define (f-111-rec n)
  (if (< n 3) 
      n
      (+ (f-111-rec (- n 1)) 
         (* 2 (f-111-rec (- n 2))) 
         (* 3 (f-111-rec (- n 3))))))

(define (f-111-iter n)
  (define (iter a b c count)
    (if (= count 0)
    a
    (iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (iter 0 1 2 n))

; ex-1.12 Pascal's triangle
(define (pascal n)
  ; calculate each item
  (define (pascal-item row col)
    (cond 
      ((< row col) #f)
      ((or (= row col)
           (= col 1)) 1)
      (else (+ (pascal-item (- row 1) (- col 1))
               (pascal-item (- row 1) col)))))
  
  ; cons each item in same line
  (define (pascal-line row col)
    (if (> col row)
        '()
        (cons (pascal-item row col) (pascal-line row (+ col 1)))))
  
  ; display all lines
  (define (pascal-display row)
    (if (> row n)
        '()
        (cons (pascal-line row 1) (pascal-display (+ row 1)))))
  
  (pascal-display 1))

; ex-1.16
(define (fast-expt b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* b a)))))
  (iter b n 1))

; ex-1.7
(define (multiply a b)  
  (cond ((= b 0) 0)
        ((even? b) (double (multiply a (halve b))))
        (else (+ a (multiply a (- b 1))))))

; ex-1.8
(define (fast-multiply a b)
  (define (iter a b count)
    (cond ((= b 0) count)
          ((even? b) (iter (double a) (halve b) count))
          (else (iter a (- b 1) (+ a count)))))
  (iter a b 0))

; [Book] prime test
(define (prime? n)
  (define (smallest-divisor n) (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

  (define (divides? a b) (= (remainder b a) 0))

  (= n (smallest-divisor n)))