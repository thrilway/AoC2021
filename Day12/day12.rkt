#lang racket

(struct cave (type outs) #:transparent)

(define (cave-add-out c out)
  (struct-copy cave c (outs (cons out (cave-outs c)))))

(define (cave-small? c)
  (equal? (cave-type c) 'small))

(define (init ip)
  (define (to-lists ip)
    (for/fold ((ls '())
               #:result (append ls (map reverse ls)))
              ((l (in-lines ip)))
      (cons (string-split l "-") ls)))
  (let loop ((cur (to-lists ip)) (acc (make-immutable-hash)))
    (cond ((null? cur) acc)
          ((hash-has-key? acc (string->symbol (caar cur)))
           (loop (cdr cur) (hash-update acc (string->symbol (caar cur)) (lambda (x) (cave-add-out x (string->symbol (cadar cur)))))))
          ((char-upper-case? (string-ref (caar cur) 0))
           (loop (cdr cur) (hash-set acc
                                     (string->symbol (caar cur))
                                     (cave 'large (list (string->symbol (cadar cur)))))))
          ((string=? (caar cur) "start")
           (loop (cdr cur)
                 (hash-set acc
                           (string->symbol (caar cur))
                           (cave 'start (list (string->symbol (cadar cur)))))))
          ((string=? (caar cur) "end")
           (loop (cdr cur)
                 (hash-set acc
                           (string->symbol (caar cur))
                           (cave 'end (list (string->symbol (cadar cur)))))))
          (else
           (loop (cdr cur)
                 (hash-set acc
                           (string->symbol (caar cur))
                           (cave 'small (list (string->symbol (cadar cur))))))))))
(define (paths caves)
  (define (doubled-path? path)
    (not (equal?
          (filter cave-small? path)
          (filter cave-small? (remove-duplicates path)))))
  (define (next-caves c h)
    (let loop ((outs (cave-outs c)) (acc '()))
      (cond ((null? outs) acc)
            ((or (not (hash-has-key? h (car outs)))
                 (equal? (car outs) 'start))
             (loop (cdr outs) acc))
            (else (loop (cdr outs) (cons (hash-ref h (car outs)) acc))))))
  (let loop ((cur (list (list (hash-ref caves 'start)))) (next '()) (done 0))
    (cond ((and (null? cur) (null? next)) done)
          ((null? cur)
           (display done)
           (newline)
           (loop next '() done))
          (else
           (let i-loop ((rem (next-caves (caar cur) caves)) (i-acc '()) (i-done 0))
             (cond
               ((null? rem) (loop (cdr cur) (append next i-acc) (+ done i-done)))
               ((equal? (cave-type (car rem)) 'end) (i-loop (cdr rem) i-acc (add1 i-done)))
               ((and
                 (equal? (cave-type (car rem)) 'small)
                 (member (car rem) (car cur))
                 (doubled-path? (car cur)))
                (i-loop (cdr rem) i-acc i-done))
               (else
                (i-loop (cdr rem) (cons (cons (car rem) (car cur)) i-acc) i-done))))))))

(define test-data #<<HERE
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
HERE
)

(define big-test-data #<<HERE
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
HERE
)

(paths (call-with-input-file "day12.txt" init))