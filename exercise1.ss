(load "fascinating.ss")

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

;(display (sumOfBigger 1 2 3))(newline)

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
;会陷入无限循环，因为是应用序求值，在调用new-if的过程中先求值所有的实参，于是无限递归
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

; (display (sqrt 0.0000000009))(newline)
; (display (sqrt 900000000000000000000))(newline)

;1.8
(define (improve-cube guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
    (if (new-good-enough? guess (improve-cube guess x))
        (improve-cube guess x)
        (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x)
    (cube-root-iter 1.0 x))

; (display (cube-root 8))(newline)

;1.9
;第一种:(+ 4 5) -> (inc (+ (dec 4) 5)) -> (inc (+ 3 5)) -> (inc (inc (+ (dec 3) 5))) 
;    ---> (inc (inc (inc (inc (inc (+ 0 5)))))) ->9 计算过程为递归
;第二种:(+ 4 5) -> (+ (dec 4) (inc 5)) -> (+ 3 6) ---> (+ 0 9) -> 9 计算过程为迭代


;1.10
;(A 1 10) -> (A 0 (A 1 9)) -> (A 0 (A 0 (A 1 8))) ---> (A 0 (A 0 ... 2)) ---> 1024
;(A 2 4) -> (A 1 (A 2 3) -> (A 1 (A 1 (A 2 2))) -> (A 1 (A 1 (A 1 2))) ---> (A 1 (A 1 4))
;    -> (A 1 (A 0 (A 1 3))) -> (A 1 16) -> 65536
;(A 3 3) -> (A 2 (A 3 2)) -> (A 2 (A 2 (A 3 1))) ... -> 65536
;(f n) -> 2n    (g n) -> 2^n    (h n) -> (g (h n-1))-> 2^(h n-1) -> 2^2^(h n-2) ---> 2^2...^2
(define (A x y)
    (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; (display (A 1 10))(newline)
; (display (A 2 4))(newline)
; (display (A 3 3))(newline)


;1.11
;递归
(define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
;迭代
(define (f n)
    (define (f-iter a b c count)
        (if (= count 0)
            c
            (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
    (f-iter 2 1 0 n))


;1.12
(define (pascal row col)
    (cond ((> col row) 0)
        ((= col 1) 1)
        (else (+ (pascal (- row 1) (- col 1)) 
                (pascal (- row 1) col)))))
; (display (pascal 2 2))(newline)
; (display (pascal 3 2))(newline)
; (display (pascal 4 3))(newline)
; (display (pascal 4 4))(newline)
; (display (pascal 5 3))(newline)
;迭代版本使用组合数公式即可，略


;1.13
;数归证已给条件即可


;1.14
;                     (cc 11 5)
;                 (cc 11 4)+(cc -39 5)
;             (cc 11 3)+(cc -14 4)+0
;         (cc 11 2)+(cc 1 3)+0
;     (cc 11 1)+(cc 6 2)+(cc 1 2)+(cc -9 3)
; (cc 11 0)+(cc 10 1)+(cc 6 1)+(cc 1 2)+(cc 1 1)+(cc -3 2)+0
; 下略
;空间O(n),时间O(2^n)


;1.15
;12.15/3=4.05,4.05/3=1.35,1.35/3=0.45,0.45/3=0.15,0.15/3=0.05,故调用了5次
;空间O(loga),时间O(loga)


;1.16
(define (even? n)
    (= (remainder n 2) 0))

(define (expt-iter b count product)
    (cond ((= count 0) product)
        ((even? count) (expt-iter (square b) (/ count 2) product))
        (else (expt-iter b (- count 1) (* product b)))))

(define (expt b n)
    (expt-iter b n 1))
; (display (expt 2 10))(newline)
; (display (expt 2 3))(newline)
; (display (expt 3 3))(newline)


;1.17
(define (multiply a b)
    (cond ((= b 0) 0)
        ((even? b) (* 2 (multiply a (/ b 2))))
        (else (+ a (multiply a (- b 1))))))
; (display (multiply 4 5))(newline)
; (display (multiply 3 3))(newline)


;1.18
(define (fast-multiply-iter a b product)
    (cond ((= b 0) product)
        ((even? b) (fast-multiply-iter (* 2 a) (/ b 2) product))
        (else (fast-multiply-iter a (- b 1) (+ product a)))))

(define (fast-multiply a b)
    (fast-multiply-iter a b 0))
; (display (fast-multiply 4 5))(newline)
; (display (fast-multiply 3 3))(newline)


;1.19
;计算得p`=p^2+q^2,q`=2pq+q^2
;详见https://sicp.readthedocs.io/en/latest/chp1/19.html


;1.20
;正则序:(gcd 206 40) -> (gcd 40 (remainder 206 40)) 
;   -> (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;下略
;应用序每调用一次gcd遍调用一次remainder，故5次


;1.21
(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

; (display (smallest-divisor 199))(newline)   ;199
; (display (smallest-divisor 1999))(newline)  ;1999
; (display (smallest-divisor 19999))(newline) ;7


;1.22
(define (prime? n)
    (= n (smallest-divides n)))

;https://sicp.readthedocs.io/en/latest/chp1/22.html


;1.23
(define (next n)
    (if (= n 2)
        3
        (+ n 2)))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;1.24
;https://sicp.readthedocs.io/en/latest/chp1/24.html


;1.25
;显然不行，这种方法会计算大数的大次幂，非常慢，并且容易溢出


;1.26
;由于是应用序求值，在改动的部分(expmod base (/ exp 2) m)重复计算了两次


;1.27
(define (check n)
    (define (try-it a)
        (= (expmod a n n) a))
    (define (check-iter a)
        (cond ((= a n) #t)
            ((try-it a) (check-iter (+ a 1)))
            (else #f)))
    (check-iter 1))
; (display (check 561))(newline)
; (display (check 1105))(newline)
; (display (check 1729))(newline)
; (display (check 2465))(newline)
; (display (check 2821))(newline)
; (display (check 6601))(newline)


;1.28
;见https://sicp.readthedocs.io/en/latest/chp1/28.html



(exit)