#lang racket

(require (for-syntax syntax/parse)
         advent-of-code
         threading)

(define input-stream
  (~> (open-aoc-input (find-session) 2022 1 #:cache #t)
      (in-port read-line _)
      sequence->stream))

(define-match-expander ->num
  (syntax-parser
    [(->num pat) #'(? string? (app string->number (? number? pat)))]))

(define (process in cur-elf max-elf)
  (match in
    [(stream* (->num n) rest) (process rest (+ n cur-elf) max-elf)]
    [(stream* "" rest) (process rest 0 (max max-elf cur-elf))]
    [(? stream-empty?) max-elf]))

(process input-stream 0 0)

