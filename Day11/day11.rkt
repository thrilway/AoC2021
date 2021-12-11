#lang racket
(require "day11-data.rkt")
(struct octo (x y v flashed?) #:transparent)
(define (octo-coord o)
  (cons (octo-x o) (octo-y o)))
(define (octo-increment o)
  (if (equal? 9 (octo-v o))
      (struct-copy octo o (v 0) (flashed? #t))
      (struct-copy octo o (v (add1 (octo-v o))))))

(define (octo-add o n)
  (let loop ((i 0) (oct o))
    (if (>= i n)
        oct
        (loop (add1 i) (octo-increment oct)))))

(define (octo-adjacent? o1 o2)
  (and
   (not (equal? o1 o2))
   (<= (abs (- (octo-x o1) (octo-x o2))) 1)
   (<= (abs (- (octo-y o1) (octo-y o2))) 1)))

(define (grid-ref x y grid)
    (car
     (filter (lambda (o) (and (equal? x (octo-x o)) (equal? y (octo-y o)))) grid)))

(define (print-grid grid)
  (let y-loop ((y 0) (acc ""))
    (if (> y 9)
        (display acc)
        (let x-loop ((x 0) (x-acc ""))
          (if (> x 9)
              (y-loop (add1 y) (string-append acc x-acc "\n"))
              (x-loop (add1 x) (string-append x-acc
                                              (number->string (octo-v (grid-ref x y grid))))))))))
(define (count-flashes grid)
  (length (filter octo-flashed? grid)))

(define (new-flash? o1 o2)
  (and (not (octo-flashed? o1))
       (octo-flashed? o2)))

(define (reset-grid grid)
  (let loop ((rem grid) (acc '()))
    (cond ((null? rem) acc)
          ((octo-flashed? (car rem)) (loop (cdr rem) (cons (struct-copy octo (car rem) (v 0) (flashed? #f)) acc)))
          (else (loop (cdr rem) (cons (car rem) acc))))))

(define (init ip)
  (define (char->number ch)
    (if (char-numeric? ch)
        (string->number (string ch))
        ch))
  (let loop ((cur (read-line ip))
             (y 0)
             (acc '()))
    (if (eof-object? cur)
        acc
        (for/fold ((a acc)
                   #:result (loop (read-line ip) (add1 y) a))
                  ((v (in-string cur))
                   (x (in-naturals)))
          (cons (octo x y (char->number v) #f) a)))))

(define (step grid)
  (define (ripple g)
    (let loop ((rem g) (acc '()) (new-flashes (filter octo-flashed? g)) (prev g))
        (cond ((null? new-flashes)
               (cons (reset-grid rem)
                     (length (filter octo-flashed? rem))))
              ((null? rem) (loop acc '() (filter (lambda (o) (new-flash? (grid-ref (octo-x o) (octo-y o) prev) o)) acc) acc))
              (else
               (loop (cdr rem)
                     (cons (octo-add (car rem) (length (filter (lambda (x) (octo-adjacent? (car rem) x)) new-flashes))) acc)
                     new-flashes
                     prev)))))     
    (for/fold ((g '())
               #:result (ripple g))
              ((o (in-list grid)))
      (cons (octo-increment o) g)))
              
(define (go grid)
  (for/fold ((g grid)
             (sum 0)
             #:result sum)
            ((i (in-naturals)))
    #:break (>= i 100)
    (let ((next (step g)))
      (values (car next)
              (+ sum (cdr next))))))

(define test-data #<<HERE
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
HERE
  )



(go (call-with-input-string data init))