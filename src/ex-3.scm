; description: my SICP exercise of charpet 3
; author: Waverim
; date: Mar 7, 2014

; function maybe useful
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (inc x) (+ x 1))
(define (average x y) (/ (+ x y) 2))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter f s)
  (cond ((null? s) (quote ()))
        ((f (car s)) (cons (car s)
                           (filter f (cdr s))))
        (else (filter f (cdr s)))))

; ex-3.1 accumulator
(define (make-accumulator sum)
  (lambda (x)
    (begin (set! sum (+ sum x))
           sum)))

; ex-3.2 monitor
(define (make-monitored f)
  (define count 0)
  (define (mf message)
    (cond ((eq? message 'how-many-calls?) count)
          ((eq? message 'reset-count) (set! count 0))
          (else (set! count (+ count 1))
                (f message))))
  mf)

; ex-3.3 make-account with password
; ex-3.4 make-account with alert
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define count 0)
  (define (call-the-cops) (lambda (x) "POLICE!"))
  (define (dispatch psw m)
    (if (not (eq? psw password))
        (if (= count 7)
            (call-the-cops)
            (begin (set! count (+ count 1))
                   (lambda (x) "Incorrect password")))
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (lambda (x) "Unkown request")))))
  dispatch)

; ex-3.5 Monte Carlo integration
; function in the book
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) 
           (/ trials-passed trials))
          ((experiment) 
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else 
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

; random
; reference from http://stackoverflow.com/a/14675103
(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (modulo (+ (* seed a) c) m)))
      (/ seed m))))

(define (P x y)
  (< (+ (square (- x 5)) (square (- x 7)))
     (square 3)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials 
               (lambda () 
                 (P (random-in-range x1 x2)
                    (random-in-range y1 y2)))))