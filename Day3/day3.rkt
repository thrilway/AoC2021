#lang racket

(define (power-consumption ip)
  (define (gamma h n)
    (for/fold ((byte-string "")
               #:result (string->number byte-string 2))
              ((i (in-list (sort (hash-keys h) >))))
      (if (> (hash-ref h i) (/ n 2))
          (string-append "1" byte-string)
          (string-append "0" byte-string))))

  (define (epsilon h n)
    (for/fold ((byte-string "")
               #:result (string->number byte-string 2))
              ((i (in-list (sort (hash-keys h) >))))
      (if (> (hash-ref h i) (/ n 2))
          (string-append "0" byte-string)
          (string-append "1" byte-string))))
  (let o-loop ((cur (read-line ip)) (i 0) (h (make-immutable-hash)))
    (if (eof-object? cur)
        (* (gamma h i) (epsilon h i))
        (let i-loop ((bits (string->list cur)) (j 0) (ih h))
          (cond ((null? bits) (o-loop (read-line ip) (add1 i) ih))
                ((and (hash-has-key? ih j)
                      (char=? (car bits) #\1))
                   (i-loop (cdr bits) (add1 j) (hash-update ih j add1)))
                ((char=? (car bits) #\1)
                 (i-loop (cdr bits) (add1 j) (hash-set ih j 1)))
                (else (i-loop (cdr bits) (add1 j) ih)))))))


(define (life-support ip)
  (define vecs
    (for/fold ((acc '())
               #:result acc)
              ((line (in-lines ip)))
      (cons (list->vector (string->list line)) acc)))

  (define (most-common-bit bytes pos)
    (let ((ones (length (filter (lambda (x) (char=? #\1 (vector-ref x pos))) bytes))))
      (if (>= ones (/ (length bytes) 2))
          #\1
          #\0)))

  (define (o-rating bytes)
    (let loop ((rem bytes) (i 0))
      (if (null? (cdr rem))
          (string->number (list->string (vector->list (car rem))) 2)
          (loop (filter (lambda (x) (char=? (most-common-bit rem i) (vector-ref x i))) rem) (add1 i)))))

  (define (co2-rating bytes)
    (let loop ((rem bytes) (i 0))
      (if (null? (cdr rem))
          (string->number (list->string (vector->list (car rem))) 2)
          (loop (filter (lambda (x) (not (char=? (most-common-bit rem i) (vector-ref x i)))) rem) (add1 i)))))

  (* (o-rating vecs) (co2-rating vecs)))

(call-with-input-file "day3.txt" life-support)