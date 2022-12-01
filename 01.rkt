#lang racket

(require (for-syntax syntax/parse)
         advent-of-code
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 1 #:cache #t)
       (in-port read-line)
       sequence->stream))

(define-match-expander ->num
  (syntax-parser
    [(->num pat) #'(? string? (app string->number (? number? pat)))]))

(module* part1 #f
  (define (process in cur-elf max-elf)
    (match in
      [(stream* (->num n) rest) (process rest (+ n cur-elf) max-elf)]
      [(stream* "" rest) (process rest 0 (max max-elf cur-elf))]
      [(? stream-empty?) max-elf]))

  (process input-stream 0 0))

(module* part2 #f
  (require data/heap)

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

  (process input-stream))

(module* part2a #f
  (require (only-in racket/base [< r<]))

  ;; Instead of inserting into a heap or accumulating a list just track the top
  ;; three items.
  (define empty (list 0 0 0))

  (define (insert t v)
    (define-match-expander <
      (syntax-parser
        [(< val pat) #'(? (λ (n) (r< n val)) pat)]
        [(< val) #'(< val _)]))
    (match t
      [(list _ c (< v b)) (list c b v)]
      [(list _ (< v c) a) (list c v a)]
      [(list (< v) b a) (list v b a)]
      [(list _ _ _) t]))

  (define (process in)
    (define (process1 in cur-elf top3)
      (match in
        [(stream* (->num n) rest) (process1 rest (+ n cur-elf) top3)]
        [(stream* "" rest) (process1 rest 0 (insert top3 cur-elf))]
        [(? stream-empty?) (apply + top3)]))
    (process1 in 0 empty))

  (process input-stream))

