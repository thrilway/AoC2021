#lang racket
(require math)

(define test-data "16,1,2,0,4,2,7,1,2,14")

(define (crabs-init ip)
  (let loop ((crabs (map string->number (string-split (read-line ip) ",")))
             (h  (make-immutable-hash)))
    (cond ((null? crabs) h)
          ((hash-has-key? h (car crabs)) (loop (cdr crabs) (hash-update h (car crabs) add1)))
          (else (loop (cdr crabs) (hash-set h (car crabs) 1))))))

(define (mode h)
  (for/fold ((count 0)
             (val 0)
             #:result val)
            ((p (in-hash-pairs h))
             #:when (> (cdr p) count))
    (values (cdr p) (car p))))

(define (fuel-use h dest)
  (for/fold ((fuel 0)
             #:result fuel)
            ((p (in-hash-pairs h)))
    (+ fuel
       (* (cdr p)
          (triangle-number
           (abs (- dest (car p))))))))

(define (minimize h)
  (let loop ((guess (apply max (hash-keys h))) (cur-min 0))
    (cond ((zero? cur-min) (loop (sub1 guess) (fuel-use h guess)))
          ((zero? guess) cur-min)
          ((< (fuel-use h guess) cur-min) (loop (sub1 guess) (fuel-use h guess)))
          (else (loop (sub1 guess) cur-min)))))

(let ((crabs (call-with-input-file "day7.txt" crabs-init)))
  (minimize crabs))