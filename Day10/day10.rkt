#lang racket

(define paren-pairs
  (list (cons #\( #\))
        (cons #\[ #\])
        (cons #\< #\>)
        (cons #\{ #\})))
(define open-parens
  (map car paren-pairs))

(define close-parens
  (map cdr paren-pairs))

(define (process-line line)
  (let loop ((rem (string->list line)) (stack '()))
    (cond ((null? rem) stack)
          ((member (car rem) open-parens) (loop (cdr rem) (cons (car rem) stack))) ;push open parens
          ((and (member (car rem) close-parens)
                (char=? (car rem) (cdr (assoc (car stack) paren-pairs))))
           (loop (cdr rem) (cdr stack))) ;pop open paren if it matches
          ((and (member (car rem) close-parens)
                (not (char=? (car rem) (cdr (assoc (car stack) paren-pairs)))))
           (car rem)))))

(define (error-score ip)
  (define point-values
    (hash
     #\) 3
     #\] 57
     #\} 1197
     #\> 25137))
  (let o-loop ((cur (read-line ip)) (score 0))
    (if (eof-object? cur)
        score
        (let ((res (process-line cur)))
          (if (char? res)
              (o-loop (read-line ip) (+ score (hash-ref point-values res)))
              (o-loop (read-line ip) score))))))

(define (complete-lines ip)
  (define point-values
    (hash
     #\) 1
     #\] 2
     #\} 3
     #\> 4))
  (define (median lst)
    (let ((sorted (sort lst >)) (len (length lst)))
      (if (zero? (remainder len 2))
          (let ((half (/ len 2)))
            (/ (+ (list-ref sorted half) (list-ref sorted (sub1 half))) 2))
          (list-ref sorted (floor (/ len 2))))))
  (let loop ((cur (read-line ip)) (scores '()))
    (if (eof-object? cur)
        (median scores)
        (let ((res (process-line cur)))
          (if (list? res)
              (for/fold ((sum 0)
                         #:result (loop (read-line ip) (cons sum scores)))
                        ((c (in-list res)))
                (+ (* 5 sum) (hash-ref point-values (cdr (assoc c paren-pairs)))))
              (loop (read-line ip) scores))))))


(define test-data #<<HERE
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
HERE
)


(call-with-input-file "day10.txt" complete-lines)