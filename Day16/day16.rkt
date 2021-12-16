#lang racket


(define (hex->binary hex)
  (number->string (string->number hex 16) 2)
  )

(struct packet (version id body) #:prefab)

(define (binary->packet bin)
  (let ((v (substring bin 0 3))
        (i (substring bin 3 6))
        (b (substring bin 6)))
    (packet
     (string->number v 2)
     (string->number i 2)
     b)))
             
             
(define (literal bstr)
  (let loop ((rem bstr) (acc ""))
    (if (char=? #\0 (string-ref rem 0))
       (values
        (string->number (string-append acc (substring rem 1 5)) 2)
        (substring rem 5))      
       (loop (substring rem 5) (string-append acc (substring rem 1 5))))))

(define (read-packet bin)
  (let loop ((rem bin) (stack '()) (sum 0))
    (cond ((null? rem) sum)
          ((and (null? stack)
                (char=? #\0 (string-ref rem 0)))
           'DO-SOMETHING)
          ((equal? 4 (car stack))
           (call-with-values (lambda () (literal rem))
                             (lambda (out in)
                               (begin
                                 (display out)
                                 (loop in (cdr stack) sum)))))
          )))
                      


(literal (packet-body (binary->packet (hex->binary "D2FE28"))))
