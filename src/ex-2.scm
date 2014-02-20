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