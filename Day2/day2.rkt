#lang racket

(struct pos (h d))
(struct state pos (aim))

(define (down p x)
  (state (pos-h p)
         (pos-d p)
         (+ (state-aim p) x)))

(define (up p x)
  (state (pos-h p)
       (pos-d p)
       (- (state-aim p) x)))

(define (forward p x)
  (state (+ x (pos-h p))
         (+ (pos-d p) (* (state-aim p) x))
         (state-aim p)))

(define (pilot ip)
  (for/fold ((p (state 0 0 0))
             #:result (* (pos-h p) (pos-d p)))
            ((line (in-lines ip)))
    (let ((cur (string-split line)))
      (case (car cur)
        (("up") (up p (string->number (cadr cur))))
        (("down") (down p (string->number (cadr cur))))
        (("forward") (forward p (string->number (cadr cur))))))))

(call-with-input-file
    "day2.txt" pilot)
  