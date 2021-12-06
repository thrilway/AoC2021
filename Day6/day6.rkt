#lang racket

(define (read-init ip)
  (for/fold ((h (make-immutable-hash (list '(0 . 0) '(1 . 0) '(2 . 0) '(3 . 0) '(4 . 0)
                                           '(5 . 0) '(6 . 0) '(7 . 0) '(8 . 0))))
             #:result h)
            ((i (in-list (map string->number (string-split (read-line ip) ",")))))
    (hash-update h i add1)))

(define (next-day h)
  (let loop ((i 0) (new-h (make-immutable-hash)))
    (cond ((> i 8)
           (hash-remove (hash-update new-h 6 (lambda (x) (+ x (hash-ref new-h -1)))) -1))
          ((zero? i) (loop (add1 i) (hash-set (hash-set new-h -1 (hash-ref h i)) 8 (hash-ref h i))))
          (else (loop (add1 i) (hash-set new-h (sub1 i) (hash-ref h i)))))))

(define (grow h days)
  (let loop ((day 0) (pop-h h))
    (if (equal? day days)
        (apply + (hash-values pop-h))
        (loop (add1 day) (next-day pop-h)))))

(define test-data "3,4,3,1,2")

(grow (call-with-input-file "day6.txt" read-init) 256)