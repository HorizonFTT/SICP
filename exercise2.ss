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
; (define (cdr z)
;     (z (lambda (p q) q)))


;2.5
; (define (cons a b)
;     (* (expt 2 a) (expt 3 b)))
; (define (car z)
;     (if (= 0 (remainder z 2))
;         (+ 1 (car (/ z 2)))
;         0))
; (define (cdr z)
;     (if (= 0 (remainder z 3))
;         (+ 1 (car (/ z 3)))
;         0))


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
; (display (reverse (list 1 2 3 4)))(newline)

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
;     ())

; (display (same-parity 1 2 3 4 5 6 7))(newline)


;2.21
(define (square x) (* x x))

(define (square-list items)
    (if (null? items)
        '()
        (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
    (map square items))

; (display (square-list (list 1 2 3 4 5 6 7)))(newline)


;2.22
;第一种组合顺序反了，第二种组合方式错误
;正确方式:最后将answer取反即可


;2.23
(define (for-each proc items)
    (cond ((not (null? items)) (proc (car items)) (for-each proc (cdr items)))))
; (for-each (lambda (x) (newline) (display x)) (list 1 2 3 4))


;2.24
;       (1 (2 (3 4)))
;       1           (2 (3 4))
;                   2       (3 4)
;                           3   4


;2.25
; (define l (list 1 2 (list 5 7) (list 9)))
; (display (car (cdaddr l)))(newline)
; (define l (list (list 7)))
; (display (caar l))(newline)
; (define l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (display (cadadr (cadadr (cadadr l))))(newline)


;2.26
; (1 2 3 4 5 6)
; ((1 2 3) 4 5 6)
; ((1 2 3) (4 5 6))


;2.27
(define (deep-reverse items)
    (iter items '()))

(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (if (pair? (car remained-items))
                        (deep-reverse (car remained-items))
                        (car remained-items)) result))))

; (display (deep-reverse (list (list 1 2) (list 3 4) (list 5 6))))


;2.28
(define (fringe tree)
    (cond ((null? tree)
            '())
          ((not (pair? tree))
            (list tree))
          (else
            (append (fringe (car tree))
                    (fringe (cadr tree))))))
; (display (fringe (list (list 1 2) (list 3 4))))(newline)


;2.29
(define (make-mobile left right)
    (list left right))
(define (make-branch length structure)
    (list length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))
(define (branch-weight branch)
        (let ((structure (branch-structure branch)))
            (if (pair? structure)
                (total-weight structure)
                structure))
        )
(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))
(define (branch-torque branch)
    (* (branch-length branch)
       (branch-weight branch)))
(define (mobile-balance? mobile)
    (define (branch-balance? branch)
        (if (pair? (branch-structure branch))
            (mobile-branch? (branch-structure branch))
            #t))
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
        (and
            (= (branch-torque left) (branch-torque right))
            (branch-balance? left)
            (branch-balance? right))))
;只需改动四个选择函数即可



(exit)