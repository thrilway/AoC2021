#lang racket

(define (init ip)
  (define (pad-row ls)
    (cons 9 (append ls '(9))))
  (define (pad-grid g)
    (let ((padder (make-vector (vector-length (car g)) 9)))
      (cons padder (append g (list padder)))))
  (let loop ((cur (read-line ip)) (acc '()))
    (if (eof-object? cur)
        (list->vector (pad-grid acc))
        (let i-loop ((i-cur cur) (i-acc '()))
          (if (string=? "" i-cur)
              (loop (read-line ip) (append
                                    acc
                                    (list (list->vector (pad-row i-acc)))))
              (i-loop (substring i-cur 1) (append i-acc (list (string->number (substring i-cur 0 1))))))))))

(define (adjacents c grid)
  (filter (lambda (x) (in-grid? x grid))
          (list
           (cons (car c) (add1 (cdr c)))
           (cons (car c) (sub1 (cdr c)))
           (cons (add1 (car c)) (cdr c))
           (cons (sub1 (car c)) (cdr c)))))

(define (in-grid? c grid)
  (and
   (and (>= (cdr c) 0)
        (< (cdr c) (vector-length grid)))
   (and (>= (car c) 0)
        (< (car c) (vector-length (vector-ref grid (cdr c)))))))

(define (low-point? c grid)
  (let loop ((val (grid-ref c grid)) (cmps (map (lambda (x) (grid-ref x grid)) (adjacents c grid))))
    (cond ((null? cmps) #t)
          ((>= val (car cmps)) #f)
          (else (loop val (cdr cmps))))))

(define (grid-ref c grid)
  (vector-ref
   (vector-ref grid (cdr c))
   (car c)))

(define (low-points grid)
  (let o-loop ((i 0) (acc '()))
    (if (>= i (vector-length grid))
        acc
        (let i-loop ((j 0) (j-acc acc))
          (cond ((>= j (vector-length (vector-ref grid i)))
                 (o-loop (add1 i) j-acc))
                ((low-point? (cons j i) grid)
                 (i-loop (add1 j) (cons (cons j i) j-acc)))
                (else (i-loop (add1 j) j-acc)))))))

(define (risk-level c grid)
  (add1
   (grid-ref c grid)))

(define (risk-level-sum grid)
  (for/fold ((sum 0)
             #:result sum)
            ((p (in-list (low-points grid))))
    (+ sum
       (risk-level p grid))))

(define (basin c grid)
  (let loop ((edge (list c)) (next-edge '()) (acc (list c)))
    (cond ((and (null? edge) (null? next-edge)) (remove-duplicates acc))
          ((null? edge) (loop (set->list next-edge) '() (append acc next-edge)))
          (else
           (let i-loop ((cs (adjacents (car edge) grid)) (i-acc '()))
             (cond ((null? cs) (loop (cdr edge) (append i-acc next-edge) acc))
                   ((or
                     (equal? 9 (grid-ref (car cs) grid))
                     (member (car cs) acc))
                     (i-loop (cdr cs) i-acc))
                   (else (i-loop (cdr cs) (append i-acc (list (car cs)))))))))))

(define (basin-sum b grid)
  (for/fold ((sum 0)
             #:result sum)
            ((p (in-list b)))
    (+ sum (grid-ref p grid))))

(define (biggest-basins grid)
  (let loop ((points (low-points grid)) (basins '()))
    (if (null? points)
        (take
         (sort basins
               (lambda (x y) (> (length x) (length y))))
         3)
        (loop (cdr points) (cons (basin (car points) grid) basins)))))
                                     
                 
                                                       

(define test-data #<<HERE
2199943210
3987894921
9856789892
8767896789
9899965678
HERE
  )
(let ((grid (call-with-input-file "day9.txt" init)))
  (apply * (map length (biggest-basins grid))))
