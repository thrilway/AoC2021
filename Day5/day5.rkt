#lang racket

(struct point (x y) #:transparent)
(struct line-seg (c1 c2) #:transparent)

(define (line-seg-run ls)
  (- (point-x (line-seg-c2 ls))
     (point-x (line-seg-c1 ls))))

(define (line-seg-rise ls)
  (- (point-y (line-seg-c2 ls))
     (point-y (line-seg-c1 ls))))

(define (line-seg-slope ls)
  (/ (line-seg-rise ls)
     (line-seg-run ls)))
(define (line-seg-b ls)
  (- (point-y (line-seg-c1 ls))
     (* (line-seg-slope ls) (point-x (line-seg-c1 ls)))))

(define (inclusive-range start stop)
  (let loop ((i (min start stop)) (j (max start stop)) (acc '()))
    (if (> i j)
        acc
        (loop (add1 i) j (cons i acc)))))

(define (line-seg-overlaps ls)
  (let ((rise (line-seg-rise ls))
        (run (line-seg-run ls)))
    (cond ((zero? run)
           (map (lambda (y) (point (point-x (line-seg-c1 ls)) y))
                (inclusive-range (point-y (line-seg-c1 ls)) (point-y (line-seg-c2 ls)))))
          ((zero? rise)
           (map (lambda (x) (point x
                                   (point-y (line-seg-c1 ls))))
                (inclusive-range (point-x (line-seg-c1 ls)) (point-x (line-seg-c2 ls)))))
          (else (let ((m (line-seg-slope ls)) (b (line-seg-b ls)))
                  (let loop ((xs (inclusive-range (point-x (line-seg-c1 ls)) (point-x (line-seg-c2 ls))))
                             (acc '()))
                    (if (null? xs)
                        acc
                        (let ((y (+ (* m (car xs)) b)))
                          (if (integer? y)
                              (loop (cdr xs) (cons (point (car xs) y) acc))
                              (loop (cdr xs) acc))))))))))

(define (read-in ip)
  (define (read-line line)
    (let ((endpoints (string-split line " -> ")))
      (line-seg
             (apply point (map string->number (string-split (car endpoints) ",")))
             (apply point (map string->number (string-split (cadr endpoints) ","))))))
  (for/fold ((vent-lines '())
             #:result vent-lines)
            ((line (in-lines ip)))
    (cons (read-line line) vent-lines)))

(define (day5 lines)
  (define overlaps (make-hash))
  (define (record-line vl)
    (for ((p (in-list (line-seg-overlaps vl))))
      (if (hash-has-key? overlaps p)
          (hash-update! overlaps p add1)
          (hash-set! overlaps p 1))))
  (for ((vl (in-list lines)))
    (record-line vl))
  (length (filter (lambda (x) (> x 1 )) (hash-values overlaps))))

(day5 (call-with-input-file "day5.txt" read-in))
            
