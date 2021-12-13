#lang racket

(define (dimensions ds)
  (let loop ((rem ds) (width 0) (height 0))
    (if
     (null? rem)
     (cons width height)
     (loop (cdr rem) (max width (caar rem)) (max height (cdar rem))))))

(define (blank-sheet dim)
  (make-vector (add1 (cdr dim))
               (make-vector (add1 (car dim)) #f)))

(define (mark-sheet! sheet coord)
  (let ((new-row (vector-copy (vector-ref sheet (cdr coord)))))
    (vector-set! new-row (car coord) #t)
    (vector-set! sheet (cdr coord) new-row)))

(define (draw sheet)
  (let yloop ((y 0) (acc ""))
    (if (equal? y (vector-length sheet))
        (display acc)
        (let xloop ((x 0) (row (vector-ref sheet y)) (i-acc acc))
          (cond ((equal? x (vector-length row)) (yloop (add1 y) (string-append i-acc "\n")))
                ((vector-ref row x) (xloop (add1 x) row (string-append i-acc "#")))
                (else (xloop (add1 x) row (string-append i-acc "."))))))))

(define (fold sheet instr)
  (define (layer sheet1 sheet2)
    (let yloop ((y 0))
      (if (equal? y (vector-length sheet1))
          sheet1
          (let ((row1 (vector-copy (vector-ref sheet1 y)))
                (row2 ((vector-ref sheet2 y))))
            (let x-loop ((x 0))
              (if (equal? x (vector-length row1))
                  (begin
                    (vector-set! sheet1 y row1)
                    (yloop (add1 y)))
                  (begin
                    (vector-set! row1 x (or (vector-ref row1 x)
                                            (vector-ref row2 x)))
                    (x-loop (add1 x)))))))))
                    
  (define (x-fold sheet col)
    (let loop ((y 0) (l '()) (r '()))
      (if (equal? y (vector-length sheet))
          (layer (list->vector l) (list->vector r))
          (call-with-values
           (lambda () (vector-split-at (vector-ref sheet y) col))
           (lambda (left right)
             (let* ((len (vector-length left))
                    (pad (make-list (- len (vector-length right)) #f))
                    (right-l (reverse (append (vector->list right) pad))))
               (loop
                (add1 y)
                (append l (list left))
                (append r (list
                           (list->vector right-l))))))))))
  (define (y-fold sheet row)
    (let ((blank-row (make-vector (vector-length (vector-ref sheet 0)) #f)))
      (let loop ((top (vector->list (vector-take sheet row))) (bottom (vector->list (vector-drop sheet (add1 row)))))
        (cond ((equal? (length top) (length bottom))
               (layer (list->vector top) (list->vector (reverse bottom))))
              ((> (length top) (length bottom))
               ;START HERE
               
                                                              
       (let* ((blank-row (make-vector (vector-length (vector-ref bot 0)) #f))
              (pad (make-list (- row (vector-length bot)) blank-row))
              (bot-l (append (vector->list bot) pad)))
         (layer top
                (list->vector (reverse bot-l)))))))
  (if (equal? (car instr) "y")
      (y-fold sheet (cdr instr))
      (x-fold sheet (cdr instr))))
                      

(define (init ip)
  (define (fill-sheet d)
    (let ((sheet (blank-sheet (dimensions d))))
      (let loop ((rem d))
        (if (null? rem)
            sheet
            (begin
              (mark-sheet! sheet (car rem))
              (loop (cdr rem)))))))
  (define (dots ip)
    (let loop ((cur (read-line ip)) (acc '()))
      (if
       (equal? "" cur)
       acc
       (let ((coord (map string->number (string-split cur ","))))
         (loop (read-line ip) (cons (cons (car coord) (cadr coord)) acc))))))
  (define (folds ip)
    (let loop ((cur (read-line ip)) (acc '()))
      (if (eof-object? cur)
          (reverse acc)
          (let ((instr (string-split (substring cur 11) "=")))
            (loop (read-line ip) (cons (cons (car instr) (string->number (cadr instr))) acc))))))
  (values (fill-sheet (dots ip)) (folds ip)))

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

(call-with-values (lambda () (call-with-input-string test-data init)) (lambda (x y) (fold x (car y))))