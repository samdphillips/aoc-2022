#lang racket

(require (for-syntax syntax/parse)
         advent-of-code
         data/heap
         threading)

(define input-stream
  (~> (open-aoc-input (find-session) 2022 1 #:cache #t)
      (in-port read-line _)
      sequence->stream))

(define-match-expander ->num
  (syntax-parser
    [(->num pat) #'(? string? (app string->number (? number? pat)))]))

(define (process in)
  (define all-elves (make-heap >))
  (define (process1 in cur-elf)
    (match in
      [(stream* (->num n) rest) (process1 rest (+ n cur-elf))]
      [(stream* "" rest)
       (heap-add! all-elves cur-elf)
       (process1 rest 0)]
      [(? stream-empty?)
       (for/sum ([e (in-heap/consume! all-elves)] [_i 3]) e)]))
  (process1 in 0))

(process input-stream)

