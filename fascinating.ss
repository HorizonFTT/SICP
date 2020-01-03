;O(logn)斐波那契
(define (square n)
    (* n n))

(define (fib n)
    (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
    (cond ((= count 0) b)
        ((even? count) (fib-iter a b 
                                    (+ (square p) (square q))
                                    (+ (* 2 p q) (square q))
                                    (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b q) (* a q))
                        p q (- count 1)))))


;GCD
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))


;快速幂取模
(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))


;序列操作
(define (filter predicate sequence)
    (cond ((null? sequence) '())
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ low 1) high))))

;嵌套映射
(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

;(exit)