(load "fascinating.ss")

;2.1
(define (make-rat n d)
    (let ((g (abs (gcd n d))))
        (if (< d 0)
            (cons (- 0 (/ n g)) (- 0 (/ d g)))
            (cons (/ n g) (/ d g)))))
(define (print-rat x)
    (newline)
    (display (car x))
    (display "/")
    (display (cdr x)))
; (print-rat (make-rat 1 (- 0 3)))


;2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment a b) (cons a b))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))
(define (midpoint-segment l)
    (make-point (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2)
        (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ".")
    (display (y-point p))
    (display ")"))
; (print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))


;2.3
;一种两条边,一种两个点,再写获取长宽的函数，即可，下略


;2.4
(define (cdr z)
    (z (lambda (p q) q)))


;2.5
(define (cons a b)
    (* (expt 2 a) (expt 3 b)))
(define (car z)
    (if (= 0 (remainder z 2))
        (+ 1 (car (/ z 2)))
        0))
(define (cdr z)
    (if (= 0 (remainder z 3))
        (+ 1 (car (/ z 3)))
        0))


;2.6
; one:lambda (f) (lambda (x) (f x))
; https://sicp.readthedocs.io/en/latest/chp2/6.html


;2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (min a b))
(define (upper-bound x) (max a b))


;2.8
(define (sub-interval x y)
    (make-interval (- (upper-bound x) (lower-bound y)) (- lower-bound x) (upper-bound y)))


;2.9
;难,暂略
;2.17
(define (last-pair items)
    (cond ((null? items) '())
        ((null? (cdr items)) items)
        (else (last-pair (cdr items)))))


;2.18
(define (reverse items)
    (iter items '()))

(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result))))


;2.19
(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+ (cc amount
                   (except-first-denomination coin-values))
               (cc (- amount
                      (first-denomination coin-values))
                   coin-values)))))
(define (no-more? items) (null? item))
(define (first-denomination items) (car items))
(define (except-first-denomination items) (cdr items))
;逆序不影响结果


;2.20

; (define (same-parity x . z)
;     (if (= (remainder x 2) (remainder (car z) 2))
;         (cons x (cons (car z) (same-parity x (cdr z))))
;         (cons x (same-parity x (cdr z)))))

; (same-parity 1 2 3 4 5 6 7)
;bug:递归会产生((3 4 5 6 7))导致报错


;2.21
(exit)