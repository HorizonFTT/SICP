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

(exit)