#lang racket
(require racket/match)

(define (init ip)
  (let loop ((cur (read-char ip)) (queue '()) (acc '()))
    (cond ((eof-object? cur) (append acc queue))
          ((char=? cur #\newline) (loop (read-char ip) '() (append acc queue)))
          ((char=? cur #\[) (let ((p (init ip)))
                              (loop (read-char ip) (append queue (list p)) acc)))
          ((char=? cur #\]) (apply cons queue))
          ((char-numeric? cur) (loop (read-char ip) (append queue (list (string->number (string cur)))) acc))
          (else (loop (read-char ip) queue acc)))))

(define (pair->string pair)
  (cond ((null? pair) "")
        ((number? pair) (number->string pair))
        ((pair? pair) (format "[~a, ~a]" (pair->string (car pair)) (pair->string (cdr pair))))))

(define (recursive-ref pair idx)
  (define (go p (i 0))
    (cond ((null? p) (list i))
          ((and (not (equal? i idx))
                (not (pair? p))) (list (add1 i)))
          ((and (equal? i idx) (not (pair? p))) p)          
          (else (let ((res (go (car p) i)))
                  (if (pair? res)
                      (go (cdr p) (car res))
                      res)))))
  (go pair))
(define (recursive-length pair)
  (define (go p i)
    (cond ((null? p) i)
          ((not (pair? p)) (add1 i))
          (else (let ((j (go (car p) i)))
                  (go (cdr p) j)))))
  (go pair 0))

(define (recursive-update pair idx proc)
  (define (go p i)
    (cond
        ((null? p) (values p i))
        ((and (not (equal? i idx))
              (not (pair? p)))
         (values p (add1 i)))
        ((and (equal? i idx) (not (pair? p))) (values (proc p) '()))
        (else (let-values (((res j) (go (car p) i)))
                (if (null? j)
                    (values
                     (cons res (cdr p))
                     j)
                    (let-values (((cdr-res k) (go (cdr p) j)))
                      (values
                       (cons res cdr-res)
                       k)))))))
  (call-with-values (lambda () (go pair 0)) (lambda (x y) x)))

(define (explode pair)
  (define (find-exploding p i d)
    (cond
      ((null? p) (values p i))
      ((number? p) (values p (add1 i)))
      ((and (>= d 4)
            (pair? p)
            (number? (car p))
            (number? (cdr p))) (values 0 (cons i p)))
      (else (let-values (((new-car j) (find-exploding (car p) i (add1 d))))
              (if (pair? j)
                  (values
                   (cons new-car (cdr p))
                   j)
                  (let-values (((new-cdr k) (find-exploding (cdr p) j (add1 d))))
                    (values
                     (cons new-car new-cdr)
                     k)))))))
  (define (ripple pair idx vals)
    (let ((temp (recursive-update pair (add1 idx) (lambda (x) (+ x (cdr vals))))))
      (cond ((not temp) (recursive-update pair (sub1 idx) (lambda (x) (+ x (car vals)))))
            ((zero? idx) temp)
            (else (recursive-update temp (sub1 idx) (lambda (x) (+ x (car vals))))))))
  (let-values (((res id-vals) (find-exploding pair 0 0)))
    (if (number? id-vals)
        #f
        (ripple res (car id-vals) (cdr id-vals)))))

(define (split pair)
  (define (go p i)
    (cond
      ((null? p) (values p i))
      ((and
            (not (null? i))
            (number? p)
            (>= p 10))
           (values
            (cons (floor (/ p 2))
                  (ceiling (/ p 2)))
            '()))
          ((number? p)
           (values p (add1 i)))
          (else (let-values (((new-car j) (go (car p) i)))
                  (if (null? j)
                      (values
                       (cons new-car (cdr p))
                       j)
                      (let-values (((new-cdr k) (go (cdr p) j)))
                        (values (cons new-car new-cdr) k)))))))
  (let-values (((next flag) (go pair 0)))
    (if (null? flag)
        next
        #f)))


(define (reduce pair)
  (let loop ((p pair))
    (let ((exploded (explode p)))
      (if exploded
          (loop exploded)
          (let ((splitted (split p)))
            (if splitted
                (loop splitted)
                p))))))

(define (fn-sum lst)
  (display "  ")
  (let loop ((cur (reduce (car lst))) (rem (cdr lst)))
    (display (pair->string cur))
    (newline)           
    (if (null? rem)
        cur
        (begin
          (newline)
          (display "  ")
          (display (pair->string cur))
          (newline)
          (display "+ ")
          (display (pair->string (car rem)))
          (newline)
          (display "= ")
          (loop (reduce (cons cur (car rem))) (cdr rem))))))

(define (mag pair)
  (if (number? pair)
      pair
      (+ (* 3 (mag (car pair))) (* 2 (mag (cdr pair))))))

(define test-data #<<HERE
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
HERE
  )

(define small-data "[[[[4,3],4],4],[7,[[8,4],9]]]\n[1,1]")
;(pair->string '(((0 4 . 5) 0 . 0) ((4 . 5) 2 . 6) 9 . 5))
(fn-sum (call-with-input-string test-data init))

;(fn-sum (call-with-input-string small-data init))