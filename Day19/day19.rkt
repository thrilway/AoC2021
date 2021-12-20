#lang racket

(struct coord (x y z) #:prefab)
(struct orientation (x y z) #:prefab)
(struct scanner coord (id o beacons) #:prefab)

(define (coord-farther? c1 c2)
  (> (apply + (map abs (list (coord-x c1) (coord-y c1) (coord-z c1))))
     (apply + (map abs (list (coord-x c2) (coord-y c2) (coord-z c2))))))
(define (coord-diff c1 c2)
  (coord (- (coord-x c1) (coord-x c2))
         (- (coord-y c1) (coord-y c2))
         (- (coord-z c1) (coord-z c2))))

(define (coord-translate c diff)
  (coord (+ (coord-x c1) (coord-x diff))
         (+ (coord-y c1) (coord-y diff))
         (+ (coord-z c1) (coord-z diff))))

(define (rotate1-coord c axis)
  (case axis
    ('z (struct-copy coord c (x (- (coord-y c))) (y (coord-x c))))
    ('y (struct-copy coord c (x (- (coord-z c))) (z (coord-x c))))
    ('x (struct-copy coord c (y (- (coord-z c))) (z (coord-y c))))))

(define (rotate-coord c axes)
  (let loop ((rem axes) (a '(x y z)) (acc c))
    (if (null? rem)
        acc
        (let i-loop ((i-rem (car rem)) (i-acc acc))
          (if (zero? rem)
              (loop (cdr rem) (cdr a) i-acc)
              (i-loop (sub1 i-rem) (rotate1-coord i-acc (car a))))))))

(define dirs
  (for*/fold ((dirs '())
              #:result dirs)
             ((x (in-range 4))
              (y (in-range 4))
              (z (in-range 4)))
    (cons (orientation x y z) dirs)))

(define (init ip)
  (let loop ((cur (read-line ip)) (scnr 0) (scnr-acc '()) (acc (hash)))
    (if (eof-object? cur)
        acc
        (let ((scnr-match (regexp-match #rx"--- scanner ([0-9]*) ---" cur))
              (coord-match (regexp-match #rx"(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)")))
          (if scnr-match
              (loop (read-line ip) (string->number (cadr scnr-match)) scnr-acc acc)
              (if coord-match
                  (loop (read-line ip) scnr (cons (apply coord (map string->number (cdr coord-match))) scnr-acc) acc)
                  (loop (read-line ip) 0 '() (hash-set acc scnr (sort scnr-acc coord-farther?)))))))))

(define (list-intercept l1 l2)
  (let loop ((rem l2) (acc '()))
    (cond ((null? rem) acc)
          ((member (car rem) l1) (loop (cdr rem) (cons (car rem) acc)))
          (else (loop (cdr rem) acc)))))
  

(define (overlaps s1 s2)
  (for/fold ((olaps '())
             #:result olaps)
            ((axis (in-list '(x y z))))
    (for*/fold ((olaps-axis '())
               #:result (map (lambda (x) (cons (cons axis (car x)) (cdr x))) olaps-axis))
              ((rot (in-list '(0 1 2 3))))
      (for/fold ((olaps-rot '())
                 #:result (map (lambda (x) (cons (cons rot (car x)) (cdr x))) olaps-axis))
               