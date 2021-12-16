#lang racket

(struct dot (x y) #:prefab)

(define (fold dots instr)
  (define (translate c l)
    (- c (* 2 (- c l))))
  (let ((axis (car instr)) (at (cdr instr)))
    (let loop ((rem dots) (acc '()))
      (cond ((null? rem) (remove-duplicates acc))
            ((and (equal? "y" axis)
                  (< (dot-y (car rem)) at))
             (loop (cdr rem) (cons (car rem) acc)))
            ((and (equal? "y" axis)
                  (> (dot-y (car rem)) at))
             (loop (cdr rem) (cons
                              (struct-copy dot (car rem) (y (translate (dot-y (car rem)) at)))
                              acc)))
            ((and (equal? "x" axis)
                  (< (dot-x (car rem)) at))
             (loop (cdr rem) (cons (car rem) acc)))
            ((and (equal? "x" axis)
                  (> (dot-x (car rem)) at))
             (loop (cdr rem) (cons
                              (struct-copy dot (car rem) (x (translate (dot-x (car rem)) at)))
                              acc)))
            (else (loop (cdr rem) acc))))))

(define (dimensions ds)
  (let loop ((rem ds) (width 0) (height 0))
    (if
     (null? rem)
     (cons width height)
     (loop (cdr rem) (max width (dot-x (car rem))) (max height (dot-y (car rem)))))))

(define (draw dots)
  (let ((dim (dimensions dots)))
    (let y-loop ((y 0) (acc ""))
      (if (> y (cdr dim))
          (display acc)
          (let x-loop ((x 0) (i-acc acc))
            (cond ((> x (car dim)) (y-loop (add1 y) (string-append i-acc "\n")))
                  ((member (dot x y) dots) (x-loop (add1 x) (string-append i-acc "▮")))
                  (else (x-loop (add1 x) (string-append i-acc ".")))))))))

(define (init ip)
  (define (dots ip)
    (let loop ((cur (read-line ip)) (acc '()))
      (if
       (equal? "" cur)
       acc
       (let ((coord (map string->number (string-split cur ","))))
         (loop (read-line ip) (cons (dot (car coord) (cadr coord)) acc))))))
  (define (folds ip)
    (let loop ((cur (read-line ip)) (acc '()))
      (if (eof-object? cur)
          (reverse acc)
          (let ((instr (string-split (substring cur 11) "=")))
            (loop (read-line ip) (cons (cons (car instr) (string->number (cadr instr))) acc))))))
  (values (dots ip) (folds ip)))

(define test-data #<<HERE
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
HERE
  )

(call-with-values (lambda () (call-with-input-file "day13.txt" init))
                  (lambda (x y)
                    (let loop ((rem y) (dots x))
                      (if (null? rem)
                          (draw dots)
                          (loop (cdr rem) (fold dots (car rem)))))))