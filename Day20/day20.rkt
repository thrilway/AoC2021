#lang racket

(require racket/draw)
(require racket/gui)

(define (init ip)
  (define (read-key ip)
    (let loop ((cur (read-line ip)) (acc ""))
      (if (string=? cur "")
          acc
          (loop (read-line ip) (string-append acc cur)))))
  (define (read-image ip)
    (for/fold ((image '())
               #:result (list->vector image))
              ((line (in-lines ip)))
      (append image (list line))))
  (values
   (read-key ip)
   (read-image ip)))

(define (pad-img i n ch)
    (for/fold ((padded '())
               #:result (let ((v-pad (make-list n (make-string (string-length (car padded)) ch))))
                          (list->vector (append v-pad padded v-pad))))
              ((row (in-vector i)))
      (let ((h-pad (make-string n ch)))
        (append padded (list (string-append h-pad row h-pad))))))

(define (enhance img key)
  (define (get-surrounds i c)
    (for*/fold ((byt "")
               #:result (string->number byt 2))
              ((y (in-range (- (cdr c) 1) (+ (cdr c) 2)))
               (x (in-range (- (car c) 1) (+ (car c) 2))))
      (let ((px (img-ref i x y)))
        (cond
          ((and (not px)
                (char=? (img-ref i (car c) (cdr c)) #\#))
           (string-append byt "1"))
          ((not px) (string-append byt "0"))
          ((char=? px #\#) (string-append byt "1"))
          (else (string-append byt "0"))))))
  
  (define (img-ref i x y)
    (if (or (< y 0) (>= y (vector-length i)))
        #f
        (if (or (< x 0) (>= x (string-length (vector-ref i y))))
            #f
            (string-ref (vector-ref i y) x))))
  (for/fold ((new-i '())
             #:result (list->vector new-i))
            ((y (in-range (vector-length img))))
    (for/fold ((new-row "")
               #:result (append new-i (list new-row)))
              ((x (in-range (string-length (vector-ref img y)))))
      (let ((ch (string-ref key (get-surrounds img (cons x y)))))
        (string-append new-row (string ch))))))

(define (count img v)
  (for*/fold ((count 0)
              #:result count)
             ((row (in-vector img))
              (ch (in-string row))
              #:when (char=? ch v))
    (add1 count)))
(define (img->string img)
  (let loop ((rem (vector->list img)) (acc ""))
    (if (null? rem)
        acc
        (loop (cdr rem) (string-append acc (car rem) "\n")))))

(define (img->bmp img scale)
  (define bmp (make-object
                  bitmap%
                (* scale (string-length (vector-ref img 0)))
                (* scale (vector-length img))
                #t))
  (define white (bytes 255 0 0 0))
  (define black (bytes 255 255 255 255))
  (define (pix->square pix n)
    (let loop ((rem (* n n)) (acc (bytes)))
      (if (zero? rem)
          acc
          (loop (sub1 rem) (bytes-append acc pix)))))
  (for ((row (in-vector img))
        (y (in-naturals)))
    (for ((ch (in-string row))
          (x (in-naturals)))
      (if (char=? ch #\#)
          (send bmp set-argb-pixels (* scale x) (* scale y) scale scale (pix->square white scale))
          (send bmp set-argb-pixels (* scale x) (* scale y) scale scale (pix->square black scale)))))
  bmp)

(define (show-img img)
  (define bmp (img->bmp img 1))
  (define f (new frame% [label "Bitmap"]))
  (new message% [parent f] [label bmp])
  (send f show #t)
  )


(define (go i k n)
  (define (prepare-img)
    (let loop ((rem n) (acc i))
      (if (zero? rem)
          (pad-img acc 2 #\.)
          (loop (sub1 rem) (pad-img acc 2 #\.)))))
  (let loop ((rem 0) (acc (prepare-img)))
    (if (>= rem n)
        (begin
          (show-img acc)
          (count acc #\#))
        (loop (add1 rem) (enhance acc k)))))
            

(define test-input #<<HERE
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
HERE
  )
(let-values (((k i)
              (
               call-with-input-file "day20.txt"
               ;call-with-input-string test-input 
                init)))
  (go i k 50))