#lang racket

(define (init ip)
  (let loop ((cur (read-line ip)) (acc '()))
    (if (eof-object? cur)
        acc
        (let ((s (string-split cur "|")))
          (loop (read-line ip) (append acc
                                       (list
                                        (map string-split s))))))))

(define (unique-outputs entries)
  (define (unique-out? s)
    (case (string-length s)
      ((2 3 4 7) #t)
      (else #f)))
  (for/fold ((c 0)
             #:result c)
            ((e (in-list entries)))
    (+ c
       (length (filter unique-out? (cadr e))))))

(define (sort-string s)
  (list->string (sort (string->list s) char<?)))

(define (decode-sig-patterns patterns)
  (define (uniques patterns)
    (let loop ((rem patterns) (unknown '()) (known (make-immutable-hash)))
      (if (null? rem)
          (cons unknown known)
          (case (string-length (car rem))
            ((2) (loop (cdr rem) unknown (hash-set known "1" (car rem))))
            ((3) (loop (cdr rem) unknown (hash-set known "7" (car rem))))
            ((4) (loop (cdr rem) unknown (hash-set known "4" (car rem))))
            ((7) (loop (cdr rem) unknown (hash-set known "8" (car rem))))
            (else (loop (cdr rem) (cons (car rem) unknown) known))))))
  (define (string-difference s1 s2)
    (let ((cmp (string->list s2)))
      (let loop ((rem (string->list s1)) (acc '()))
        (cond ((null? rem) (list->string acc))
              ((not (member (car rem) cmp)) (loop (cdr rem) (cons (car rem) acc)))
              (else (loop (cdr rem) acc))))))
  (define (go unknown known)
    (if (null? unknown)
       (for/fold ((acc (make-immutable-hash))
                  #:result acc)
                 ((a (in-hash-pairs known)))
         (hash-set acc (sort-string (cdr a)) (car a)))
       (let ((cur (string-difference (hash-ref known "8") (car unknown))))
            (cond
              ((and
                (equal? 1 (string-length cur))
                (string=? "" (string-difference cur  (hash-ref known "1"))))
               (go (cdr unknown) (hash-set known "6" (car unknown))))
              ((and (hash-has-key? known "6")
                    (equal? 1 (string-length (string-difference cur (hash-ref known "6")))))
               (go (cdr unknown) (hash-set known "5" (car unknown))))
              ((and (hash-has-key? known "5")
                    (string=? cur (string-difference cur (hash-ref known "5"))))
               (go (cdr unknown) (hash-set known "9" (car unknown))))
              ((and (hash-has-key? known "5")
                    (equal? 2 (string-length cur))
                    (string=? "" (string-difference cur (hash-ref known "5"))))
               (go (cdr unknown) (hash-set known "2" (car unknown))))
              ((and (hash-has-key? known "5")
                    (equal? 1 (string-length cur))
                    (string=? "" (string-difference cur (hash-ref known "5"))))
               (go (cdr unknown) (hash-set known "0" (car unknown))))
              ((and (hash-has-key? known "5")
                    (equal? 2 (string-length cur))
                    (equal? 1 (string-length (string-difference cur (hash-ref known "5")))))
               (go (cdr unknown) (hash-set known "3" (car unknown))))
              (else (go (append (cdr unknown) (list (car unknown))) known))))))
  (let ((u (uniques patterns)))
    (go (car u) (cdr u))))

(define (decode-entries entries)
  (define (decode-entry entry)
    (let ((p (decode-sig-patterns (car entry))))
      (let loop ((rem (cadr entry)) (acc ""))
        (if (null? rem)
            (string->number acc)
            (loop (cdr rem) (string-append acc (hash-ref p (sort-string (car rem)))))))))
  (for/fold ((sum 0)
             #:result sum)
            ((e (in-list entries)))
    (+ sum (decode-entry e))))
    
(define (day8 entries)
  (for/fold ((patterns '())
             #:result patterns)
            ((e (in-list entries)))
    (cons (decode-sig-patterns (car e)) patterns))
  )

(define test-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
(define test-lines #<<HERE
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
HERE
)

(decode-entries (call-with-input-file "day8.txt" init))