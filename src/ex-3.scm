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
      (set! seed (modulo (+ (* seed a) c) m))
      (/ seed m))))

(define (P x y)
  (< (+ (square (- x 5)) (square (- x 7)))
     (square 3)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials 
               (lambda () 
                 (P (random-in-range x1 x2)
                    (random-in-range y1 y2)))))

; ex-3.7 make-joint, Just write a concept
(define (make-joint account old-psw new-psw)
  (lambda (password message)
    (if (eq? old-psw new-psw)
        (account old-psw message)
        (lamda (x) "Incorrect password"))))

; ex-3.8 Order of evaluation
(define evaluate-order
  (let ((s 1))
    (lambda (x)
      (set! s (* s x))
      s)))

; ex-3.17 redefine count-pairs
(define (count-pairs x)
  (define (iter x result)
    (if (and (pair? x)
             (not (memq x result)))
        (iter (car x)
              (iter (cdr x)
                    (cons x result)))
        result))
  (length (iter x (quote ()))))

; ex-3.18 detect loop
(define (loop? list)
  (let ((i (quote())))
    (define (iter remain-list)
      (cond ((null? remain-list) #f)
            ((eq? i (car remain-list)) #t)
            (else (set-car! remain-list i)
                  (iter (cdr remain-list)))))
    (iter list)))

; ex-3.19 Floyd's cycle-finding algorithm
; reference from http://community.schemewiki.org/
(define (floyd-loop? list)
  (define (safe? list)
    (if (pair? list)
        (cdr list)
        (quote ())))
  (define (iter a b)
    (cond ((or (not (pair? a))
               (not (pair? b)))
           #f)
          ((or (eq? a b)
               (eq? a (safe? b)))
           #t)
          (else (iter (safe? a)
                      (safe? (safe? b))))))
  (iter (safe? list)
        (safe? (safe? list))))