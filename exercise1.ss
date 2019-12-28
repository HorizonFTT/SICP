;1.1
10  ;10
(+ 5 3 4)   ;12
(- 9 1)     ;8
(/ 6 2)     ;3
(+ (* 2 4) (- 4 6))     ;6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))     ;19
(= a b)     ;#f
(if (and (> b a) (< b (* a b)))
    b
    a)      ;4
(cond ((= a 4) 6)
    ((= b 4) (+ 6 7 a))
    (else 25))      ;16
(+ 2 (if (> b a) b a))      ;6
(* (cond ((> a b) a)
        ((< a b) b)
        (else -1))
    (+ a 1))        ;16


;1.2
(/  (+ 5 4 
        (- 2 
            (- 3 
                (+ 6 
                    (/ 4 5))))) 
    (* 3 
        (- 6 2) 
        (- 2 7)))


;1.3
(define (sumOfBigger x y z)
    (cond ((and (>= x z) (>= y z)) (+ x y))
        ((and (>= x y) (>= z y)) (+ x z))
        ((and (>= y x) (>= z x)) (+ y z))
        ))

(display (sumOfBigger 1 2 3))
(newline)

;另解
(define (bigger x y)
    (if (> x y) x y))
(define (smaller x y)
    (if (> x y) y x))
(define (bigger-sum x y z)
    (+ (bigger x y) (bigger (smaller x y) z)))


;1.4
;if b>0 return a+b else return a-b


;1.5
;正则序:(test 0 (p)) -> (if (= 0 0) 0 (p)) - > (0)
;应用序:(test 0 (p)) -> (test 0 (p)) 无限循环


;1.6
;会陷入无限循环，因为是应用序求职，在调用new-if的过程中先求值所有的实参，于是无限递归
(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
        (else else-clause)))

(define (square x) (* x x))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))


;1.7
;对于很小的数,无法得出精确结果,对于很大的数会陷入无限循环
;新实现如下:
(define (new-good-enough? old new)
    (> 0.01 
        (/ (abs (- old new))
             old)))

(define (sqrt-iter guess x)
    (if (new-good-enough? guess (improve guess x))
        (improve guess x)
        (sqrt-iter (improve guess x) x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(display (sqrt 0.0000000009))
(newline)
(display (sqrt 900000000000000000000))
(newline)

;1.8
(define (improve-cube guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
    (if (new-good-enough? guess (improve-cube guess x))
        (improve-cube guess x)
        (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x)
    (cube-root-iter 1.0 x))

(display (cube-root 8))
(exit)