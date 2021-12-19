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
  (let loop ((cur (reduce (car lst))) (rem (cdr lst))) 
    (if (null? rem)
        cur
        (loop (reduce (cons cur (car rem))) (cdr rem)))))
(define (biggest-sum lst)
  (for/fold ((m 0)
            #:result m)
            ((a1 (in-list lst)))
    (for/fold ((n 0)
               #:result (max n m))
              ((a2 (in-list lst))
               #:unless (equal? a1 a2))
      (max n (mag (reduce (cons a1 a2)))))))

(define (mag pair)
  (if (number? pair)
      pair
      (+ (* 3 (mag (car pair))) (* 2 (mag (cdr pair))))))

(define test-data #<<HERE
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
HERE
)
(biggest-sum (call-with-input-file "day18.txt" init))