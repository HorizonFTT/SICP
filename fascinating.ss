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


;快速幂取模
(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))



(exit)