#lang racket

(define (count-increases ip)
  (for/fold ((incr 0)
             (prev '())
             #:result incr)
            ((n (string->number (in-lines ip))))
    (cond ((null? prev) (values incr n))
          ((> n prev) (values (add1 incr) n))
          (else (values incr n)))))

(define (count-windowed-increases ip)
  (for/fold ((incr 0)
             (prev-sum 0)
             (window '())
             #:result (if (> (apply + window) prev-sum) (add1 incr) incr))
            ((line (in-lines ip)))
    (let ((n (string->number line)))
      (if
       (equal? 3 (length window))
       (let ((sum (apply + window)))
         (cond
           ((zero? prev-sum)
            (values 0 sum (take (cons n window) 3)))
           ((> sum prev-sum) (values (add1 incr) sum (take (cons n window) 3)))
           (else (values incr sum (take (cons n window) 3)))))
       (values incr prev-sum (cons n window))))))

(call-with-input-file "input.txt"
  count-windowed-increases)