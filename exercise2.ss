(load "fascinating.ss")

;2.1
(define (make-rat n d)
    (let ((g (gcd n d)))
        (cond ((and (< n 0) (< d 0)) )))