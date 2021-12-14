#lang racket

(define (step pc rules char-c)
  (let loop ((rem pc) (acc '()) (cc char-c))
    (if (null? rem)
        (values acc cc)
        (let ((rule (assoc (caar rem) rules)))
          (if (not rule)
              (loop (cdr rem) (update-sums acc (list (car rem))) cc)
              (call-with-values (lambda () (apply-rule rule pc cc))
                                (lambda (x y)
                                  (loop (cdr rem) (update-sums acc x) y))))))))
                      
  
(define (expand pair-count rules char-count n)
  (let loop ((i 0) (acc pair-count) (cc char-count))
    (if (equal? i n)
        (spread cc)
        (call-with-values (lambda () (step acc rules cc))
                          (lambda (x y) (loop (add1 i) x y))))))

(define (spread char-counts)
  (for/fold ((low (expt 10 100))
             (high 0)
             #:result (- high low))
            ((c (in-hash-pairs char-counts)))
    (values
     (min low (cdr c))
     (max high (cdr c)))))

(define (apply-rule rule pair-count char-count)
  (let* ((old-cs (string->list (car rule)))
         (new-l (string (car old-cs) (cdr rule)))
         (new-r (string (cdr rule) (cadr old-cs))))
    (values
     (list (cons new-l (cdr (assoc (car rule) pair-count)))
           (cons new-r (cdr (assoc (car rule) pair-count))))
     (if (hash-has-key? char-count (cdr rule))
         (hash-update char-count (cdr rule)
                      (lambda (x) (+ x (cdr (assoc (car rule) pair-count)))))
         (hash-set char-count
                   (cdr rule)
                   (cdr (assoc (car rule) pair-count)))))))

(define (update-sums sums additions)
  (let ((old-h (make-hash sums)))
    (let loop ((rem additions))
    (cond ((null? rem) (hash->list old-h))
          ((hash-has-key? old-h (caar rem))
           (hash-update! old-h (caar rem) (lambda (x) (+ x (cdar rem))))
           (loop (cdr rem)))
          (else
           (hash-set! old-h (caar rem) (cdar rem))
           (loop (cdr rem)))))))


(define (init ip)
  (define (count-pairs str)
    (let loop ((i 0) (acc (hash)))
        (cond ((equal? (add1 i) (string-length str)) (hash->list acc))
              ((hash-has-key? acc (substring str i (+ i 2)))
               (loop (add1 i) (hash-update acc (substring str i (+ i 2)) add1)))
              (else (loop (add1 i) (hash-set acc (substring str i (+ i 2)) 1))))))
  (define (count-chars str)
    (let loop ((rem (string->list str)) (acc (hash)))
      (cond ((null? rem) acc)
            ((hash-has-key? acc (car rem)) (loop (cdr rem) (hash-update acc (car rem) add1)))
            (else (loop (cdr rem) (hash-set acc (car rem) 1))))))
  (define temp (read-line ip))
  (read-line ip)
  (let loop ((cur (read-line ip)) (acc '()))
    (cond ((eof-object? cur)
           (values (count-pairs temp) acc (count-chars temp))) ;RETURN
          (else
           (let ((rule (string-split cur " -> ")))
             (loop (read-line ip) (cons (cons (car rule) (string-ref (cadr rule) 0)) acc)))))))




(define test-data ;Test input
  #<<HERE
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
HERE
  )

(call-with-values
 (lambda () (call-with-input-file "day14.txt" init))
 (lambda (x y z) (expand x y z 40)))
                  
