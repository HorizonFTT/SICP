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


;2.30
(define (square-tree tree)
    (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree tree)
    (map (lambda (sub-tree)
                (if (pair? sub-tree)
                    (square-tree sub-tree)
                    (square sub-tree))) tree))
; (display (square-tree (list (list 1 2) (list 3 4))))(newline)


;2.31
(define (tree-map f tree)
    (map (lambda (sub-tree)
                (if (pair? sub-tree)
                    (tree-map f sub-tree)
                    (f sub-tree))) tree))
(define (square-tree tree) (tree-map square tree))
; (display (square-tree (list (list 1 2) (list 3 4))))(newline)


;2.32
(define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (set) (cons (car s) set)) rest)))))
; (display (subsets (list 1 2 3)))(newline)
;递归的过程，以(1 2 3)为例,先得到(()),再将3与各元素组合得到(() (3)),再将2与各元素组合
;得到(() (3) (2) (2 3)),再1即得结果


;2.33
; (define (map p sequence)
;     (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
; (define (append seq1 seq2)
;     (accumulate cons seq2 seq1))
; (define (length sequence)
;     (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;2.34
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
                0 coefficient-sequence))
; (display (horner-eval 2 (list 1 3 0 5 0 1)))(newline)


;2.35
(define (count-leaves t)
    (accumulate + 0 (map (lambda (sub-tree)
                                (if (pair? sub-tree)
                                    (count-leaves sub-tree)
                                    1)) t)))
; (display (count-leaves (list (list 1 (list 2 3)) (list (list 4 5) (list 6 7)))))(newline)


;2.36
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; (display (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))))(newline)


;2.37
(define (dot-product v w)
    (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
    (map (lambda (col) (dot-product col v)) m))
(define (transpose mat)
    (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (col) (matrix-*-vector cols col)) m)))


;2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))
;op符合结合律即可


;2.39
(define (reverse sequence)
    (fold-right (lambda (x y) (cons y x)) '() sequence))
(define (reverse sequence)
    (fold-left (lambda (x y) (append y (list x))) '() sequence))


;2.40
(define (unique-pairs n)
    (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))


;2.41
(define (unique-triples n)
    (flatmap (lambda (i) (map (lambda (j) (cons i j)) (unique-pairs (- i 1))))
                (enumerate-interval 1 n)))
(define (triple-sum-equal? sum triple)
    (= sum
       (fold-right + 0 triple)))
(define (remove-triples-not-equal sum triple)
    (filter (lambda (current-triple)
                (triple-sum-equal? sum current-triple))
            triple))
; (display (remove-triples-not-equal 11 (unique-triples 13)))(newline)


;2.42
(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                                (adjoin-position new-row k rest-of-queens))
                            (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
(define (safe? k position)
    (iter-check (car position) 
                (cdr position)
                 1))
(define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
            (if (or (= row-of-new-queen row-of-current-queen)
                    (= row-of-new-queen (+ i row-of-current-queen))
                    (= row-of-new-queen (- row-of-current-queen i)))
                #f
                (iter-check row-of-new-queen 
                            (cdr rest-of-queens)
                            (+ i 1))))))
; (for-each (lambda (pos) (begin (display pos) (newline)))(queens 8))


;2.43
;https://sicp.readthedocs.io/en/latest/chp2/43.html
(exit)