#lang racket

(define (init ip)
  (let loop ((cur (read-line ip) (acc '())))
    (if 
      (eof-object? cur)
      acc
      '())))
