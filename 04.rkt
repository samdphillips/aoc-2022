#lang racket

(require (for-syntax syntax/parse)
         advent-of-code
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 4 #:cache #t)
       (in-port read-line)
       sequence->stream))

(define-match-expander ->n
  (syntax-parser
    [(->n pat) #'(? string? (app string->number (? number? pat)))]))

(struct range (start end) #:transparent)

(define (range-contains? r v)
  (<= (range-start r) v (range-end r)))

(define (range-fully-contains? r0 r1)
  (and (range-contains? r0 (range-start r1))
       (range-contains? r0 (range-end r1))))

(define (range-overlaps? r0 r1)
  (or (range-contains? r0 (range-start r1))
      (range-contains? r0 (range-end r1))))

(define (parse-assignments s)
  (match s
    [(regexp #px"^(\\d+)-(\\d+),(\\d+)-(\\d+)$"
             (list _ (->n a) (->n b) (->n c) (->n d)))
     (values (range a b) (range c d))]))

(module* part1 #f
  (for/sum ([s input-stream]
            #:do [(define-values (r0 r1) (parse-assignments s))]
            #:when (or (range-fully-contains? r0 r1)
                       (range-fully-contains? r1 r0)))
        1))

(module* part2 #f
  (for/sum ([s input-stream]
            #:do [(define-values (r0 r1) (parse-assignments s))]
            #:when (or (range-overlaps? r0 r1) (range-overlaps? r1 r0)))
    1))
