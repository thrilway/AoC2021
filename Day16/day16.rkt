#lang racket


(define (hex->binary hex)
  (let loop ((rem (map string (string->list hex))) (acc ""))
    (if (null? rem)
        acc
        (let i-loop ((str (number->string (string->number (car rem) 16) 2)))
          (if (equal? 4 (string-length str))
              (loop (cdr rem) (string-append acc str))
              (i-loop (string-append "0" str)))))))                
             
(define (read-vers bstr)
  (lambda ()
    (let ((v (substring bstr 0 3)) (r (substring bstr 3)))
      (values r 'id (string->number v 2)))))

(define (read-id bstr)
  (lambda ()
    (let ((id (string->number (substring bstr 0 3) 2)) (r (substring bstr 3)))
      (if (equal? 4 id)
          (values r 'literal)
          (if (char=? #\0 (string-ref r 0))
                (let* ((len (string->number (substring r 1 16) 2))
                          (b (substring r 16 (+ 16 len)))
                          (new-r (substring r (+ 16 len))))
                  (values b (cons id new-r)))
                (let ((num (string->number (substring r 1 12) 2))
                      (b (substring r 12)))
                  (values b (cons id num))))))))
(define (literal bstr)
  (lambda ()
    (let loop ((rem bstr) (acc ""))
      (if (char=? #\0 (string-ref rem 0))
          (values
           (string->number (string-append acc (substring rem 1 5)) 2)
           (substring rem 5))
          (loop (substring rem 5) (string-append acc (substring rem 1 5)))))))

(define (read-packet bin (op '()) (num 0))
  (define op-table
    (hash
     0 +
     1 *
     2 min
     3 max
     5 (lambda (x y) (if (> x y) 1 0)) 
     6 (lambda (x y) (if (< x y) 1 0))
     7 (lambda (x y) (if (equal? x y) 1 0))))
  (let loop ((rem bin) (stack '()) (operands '()) (sum 0))
    (cond ((and
            (not (zero? num)) ; Operating on a fixed number of values
            (equal? num (length operands))) ; You have enough values
           (values rem (apply (hash-ref op-table op) operands) sum)) ; run the operation
          ((and (not (null? op)) ; You've got an operator
                (zero? num)      ; operation on the wohle input
                (null? stack)    
                (string=? rem (make-string (string-length rem) #\0))) ; no more packets to read
           (values (apply (hash-ref op-table op) operands) sum)) ; Evaluate it
          ((and (null? op) ; no operation
                (null? stack) 
                (string=? rem (make-string (string-length rem) #\0))) ;nothing left to read
           (car operands)) ; DONE!
          ((null? stack) ; Start
           (call-with-values
            (read-vers rem)
            (lambda (r code v)
              (loop r (cons code stack) operands (+ sum v)))))
          ((equal? 'id (car stack)) ; What kind of packet?
           (call-with-values
            (read-id rem)
            (lambda (r code)
              (loop r (cons code (cdr stack)) operands sum))))
          ((equal? 'literal (car stack)) ; read a literal and add it to the operands 
           (call-with-values (literal rem)
                             (lambda (lit r)
                               (loop r (cdr stack) (append operands (list lit)) sum))))
          ((string? (cdar stack)) ;operate on a fixed string
           (call-with-values
            (lambda () (read-packet rem (caar stack)))
            (lambda (val s)
              (loop (cdar stack) (cdr stack) (append operands (list val)) (+ sum s)))))
          ((number? (cdar stack)) ;operate on a number of packets
           (call-with-values
            (lambda () (read-packet rem (caar stack) (cdar stack)))
            (lambda (r val s)
              (loop r (cdr stack) (append operands (list val)) (+ sum s))))))))

(define test-strings
  (list
   "C200B40A82"
   "04005AC33890"
   "880086C3E88112"
   "CE00C43D881120"
   "D8005AC2A8F0"
   "F600BC2D8F"
   "9C005AC2F8F0"
   "9C0141080250320F1802104A08"))

;(map (compose read-packet hex->binary) test-strings)
(call-with-input-file "day16.txt" (compose read-packet hex->binary read-line))
