#lang racket

(struct node (val
              dist
              prev
              end?
              visited?)
              #:prefab)

(define (init ip)
  (define (seed ip)
    (for/fold ((h (hash))
               #:result h)
              ((row (in-lines ip))
               (y (in-naturals)))
      (for/fold ((h0 h)
                 #:result h0)
                ((ch (in-string row))
                 (x (in-naturals)))
        (hash-set h0 (cons x y) (node (string->number (string ch))
                                      +inf.0
                                      '()
                                      #f
                                      #f)))))
  (define (grid-dim grid)
    (for/fold ((d 0)
               #:result d)
              ((k (in-hash-keys grid)))
      (if (> (car k) d)
          (car k)
          d)))
  (define (tile grid dim)
    (define old-d (grid-dim grid))
    (for*/fold ((h (hash))
                #:result h)
               ((p (in-hash-pairs grid))
                (x (in-list (range dim)))
                (y (in-list (range dim))))
      (let ((new-x (+ (caar p) (* x (add1 old-d))))
            (new-y (+ (cdar p) (* y (add1 old-d))))
            (new-val (remainder (+ x y (node-val (cdr p))) 9)))
        (cond ((zero? new-val) (hash-set h (cons new-x new-y) (node 9 +inf.0 '() #f #f)))
              ((and (zero? new-x) (zero? new-y))
               (hash-set h (cons x y) (node new-val
                                            0
                                            '()
                                            #f
                                            #f)))
              (else (hash-set h (cons new-x new-y) (node new-val +inf.0 '() #f #f)))))))

  (let* ((g (tile (seed ip) 5))
         (d (grid-dim g)))
    (hash-update g (cons d d) (lambda (x) (struct-copy node x (end? #t))))))

(define (neighbours n grid)
  (let ((x (car n))
        (y (cdr n)))
  (let loop ((rem (list (cons (add1 x) y) (cons x (add1 y))))
               (acc '()))
      (cond ((null? rem) acc)
            ((hash-has-key? grid (car rem)) (loop (cdr rem) (cons (car rem) acc)))
            (else (loop (cdr rem) (cons (car rem) acc)))))))
        

(define (dijkstra nodes)
  (display "In the Algo")
  (newline)
  (display (hash-ref nodes '(0 . 0)))
  (newline)
  (define (min-dist nodes)
    (for/fold ((d +inf.0)
               (coords '())
               #:result coords)
              ((p (in-hash-pairs nodes)))
      (if
       (and (not (node-visited? (cdr p)))
            (< (node-dist (cdr p)) d))
       (values (node-dist (cdr p)) (car p))
       (values d coords))))
  (define (visit-node nodes)
    (lambda (n)
      (struct-copy node n (visited? #t))))
  (define (temp-dist nodes cur n)
      (+ (node-dist (hash-ref nodes cur)) (node-val (hash-ref nodes n))))
  (let loop ((cur (min-dist nodes)) (q (hash-update nodes (min-dist nodes) (visit-node nodes))))
      (cond ((node-end? (hash-ref q cur)) (node-dist (hash-ref q cur)))
             (else (let i-loop ((nexts (neighbours cur q)) (acc q))
                     (cond ((null? nexts) (loop (min-dist acc) (hash-update acc (min-dist acc) (visit-node acc))))
                           ((hash-has-key? acc (car nexts))
                            (let ((tmp (temp-dist acc cur (car nexts))))
                              (if (< tmp (node-dist (hash-ref q (car nexts))))
                                  (i-loop (cdr nexts)
                                           (hash-update acc (car nexts)
                                                        (lambda (z) (struct-copy node z
                                                                                 (dist tmp)))))
                                  (i-loop (cdr nexts) acc))))
                           (else (i-loop (cdr nexts) acc))))))))

               

(define test-data #<<HERE
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
HERE
  )



(define data (call-with-input-file "day15.txt" init))
(dijkstra data)
          

