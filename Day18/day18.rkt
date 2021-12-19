#lang racket

(require "day18-data.rkt")

(define (init ip)
  (let loop ((cur (read-char ip)) (row-acc '()) (acc '()))
    (cond ((eof-object? cur) (append acc (list (reverse row-acc))))
          ((char=? cur #\newline) (loop (read-char ip) '() (append acc (list (reverse row-acc)))))
          ((char=? cur #\[) (loop (read-char ip) (cons 'l row-acc) acc))
          ((char=? cur #\]) (loop (read-char ip) (cons 'r row-acc) acc))
          ((char-numeric? cur) (loop (read-char ip) (cons (string->number (string cur)) row-acc) acc))
          (else (loop (read-char ip) row-acc acc)))))

(define (split n)
  (list 'l (floor (/ n 2)) 
        (ceiling (/ n 2)) 'r))

(define (reduce p)
  (define (rebuild left right)
    (let loop ((rem left) (acc right))
      (if (null? rem)
          acc
          (loop (cdr rem) (cons (car rem) acc)))))
  (define (explode left right)
    (let ((l (car right)) (r (cadr right)) (new-left (cdr left)) (new-right (cdr (member 'r right))))
      (let ((l-idx (index-where new-left number?))
            (r-idx (index-where new-right number?)))
        (cond ((and l-idx r-idx)
               (rebuild (list-update new-left l-idx (lambda (x) (+ x l)))
                        (cons 0
                                (list-update new-right r-idx (lambda (x) (+ x r))))))
              (l-idx (rebuild (list-update new-left l-idx (lambda (x) (+ x l)))
                              (cons 0 new-right)))
              (r-idx (rebuild new-left
                              (cons 0
                                (list-update new-right r-idx (lambda (x) (+ x r))))))
              (else (not #t))))))
  (let loop ((rem p) (left '()) (level 0))
    (cond ((null? rem) (reverse left))
          ((equal? (car rem) 'l) (loop (cdr rem) (cons 'l left) (add1 level)))
          ((equal? (car rem) 'r) (loop (cdr rem) (cons 'r left) (sub1 level)))
          ((and (number? (car rem))
                (> level 4))
           (loop (explode left rem)
                 '()
                 0))
          ((and (number? (car rem))
                (>= (car rem) 10))
           (loop (rebuild left (append (split (car rem)) (cdr rem))) '() 0))
          ((number? (car rem)) (loop (cdr rem) (cons (car rem) left) level))
          (else (not #t)))))
(define (fish-sum lst)
  (let loop ((cur (car lst)) (rem (cdr lst)))
    (if (null? rem)
        (reduce cur)
        (loop (append '(l) (reduce cur) (car rem) '(r)) (cdr rem)))))
(define (fn->pair fn)
  (let loop ((rem fn) (left '()))
    (match rem
      ((list 'l
             (? (lambda (x) (or (pair? x) (number? x))) l)
             (? (lambda (x) (or (pair? x) (number? x))) r)
             'r
             _ ...)
       (loop (append left (list (cons l r)) (
             
    (cond ((equal? (car fn) 'r) )
          ((equal? (car fn) 'l) (apply cons (fn->pair (cdr fn))))
          ((number? (car fn)) (cons (car fn) (fn->pair (cdr fn))))))
(define (mag fn)
  (if (number? fn)
      fn
      (+ (* 3 (mag (car fn))) (* 2 (cdr fn)))))
          

;(mag (fish-sum (call-with-input-string big-test init)))
(fn->pair (car (call-with-input-string "[[[[[9,8],1],2],3],4]" init)))
                   