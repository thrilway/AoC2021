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
          (let ((op
                 (case id
                    ((0) 'plus)
                    ((1) 'minus)
                    ((2) 'mini)
                    ((3) 'maxi)
                    ((5) 'gt)
                    ((6) 'lt)
                    ((7) 'equ))))
            (if (char=? #\0 (string-ref r 0))
                (let* ((len (string->number (substring r 1 16) 2))
                          (b (substring r 16 (+ 16 len)))
                          (new-r (substring r (+ 16 len))))
                  (values b (cons op new-r)))
                (let ((num (string->number (substring r 1 12) 2))
                      (b (substring r 12)))
                  (values b (cons op num)))))))))
(define (literal bstr)
  (lambda ()
    (let loop ((rem bstr) (acc ""))
      (if (char=? #\0 (string-ref rem 0))
          (values
           (string->number (string-append acc (substring rem 1 5)) 2)
           (substring rem 5))
          (loop (substring rem 5) (string-append acc (substring rem 1 5)))))))
(define (call op x)
  (cond
    ((eqv? op 'plus) (apply + x))
    ((eqv? op 'minus) (apply * x))
    ((eqv? op 'mini) (apply min x))
    ((eqv? op 'maxi) (apply max x))
    ((eqv? op 'gt) (if (apply > x) 1 0))
    ((eqv? op 'lt) (if (apply < x) 1 0))
    ((eqv? op 'equ) (if (apply equal? x) 1 0))))

(define (read-packet bin (op '()) (num 0))
  (let loop ((rem bin) (stack '()) (operands '()) (sum 0))
    (cond ((and
            (not (zero? num))
            (equal? num (length operands)))
           (values rem (call op operands) sum))
          ((and (not (null? op))
                (zero? num)
                (null? stack)
                (string=? rem (make-string (string-length rem) #\0)))
           (values (call op operands) sum))
          ((and (null? op)
                (null? stack)
                (string=? rem (make-string (string-length rem) #\0)))
           (car operands))
          ((null? stack)
           (call-with-values
            (read-vers rem)
            (lambda (r code v)
              (loop r (cons code stack) operands (+ sum v)))))
          ((equal? 'id (car stack))
           (call-with-values
            (read-id rem)
            (lambda (r code)
              (loop r (cons code (cdr stack)) operands sum))))
          ((equal? 'literal (car stack))
           (call-with-values (literal rem)
                             (lambda (lit r)
                               (loop r (cdr stack) (append operands (list lit)) sum))))
          ((string? (cdar stack))
           (call-with-values
            (lambda () (read-packet rem (caar stack)))
            (lambda (val s)
              (loop (cdar stack) (cdr stack) (append operands (list val)) (+ sum s)))))
          ((number? (cdar stack))
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