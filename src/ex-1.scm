; description: my SICP exercise of charpet 1
; author: Waverim
; date: Feb 16, 2014

; function maybe useful
(define (square x) (* x x))
(define (cube x) (* x x x))

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
    