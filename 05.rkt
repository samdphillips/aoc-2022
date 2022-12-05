#lang racket

(require advent-of-code
         syntax/parse/define
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 5 #:cache #t)
       (in-port read-line)
       sequence->stream))

(define (empty-string? s)
  (zero? (string-length s)))

(define (stream-split s pred?)
  (define (stream-pred? s)
    (pred? (stream-first s)))
  (define (first s)
    (cond
      [(stream-empty? s) empty-stream]
      [(stream-pred? s) empty-stream]
      [else (stream-cons (stream-first s) (first (stream-rest s)))]))
  (define (second s)
    (cond
      [(stream-empty? s) empty-stream]
      [(stream-pred? s) (stream-rest s)]
      [else (stream-lazy (second (stream-rest s)))]))
  (values (first s) (second s)))

(define (parse-column s n)
  (define i (add1 (* 4 n)))
  (string-ref s i))

(define (parse-row s stacks)
  (cond
    [(char-numeric? (parse-column s 0))
     (for ([i (in-naturals)] [stk (in-vector stacks)])
       (vector-set! stacks i (reverse stk)))
     #f]
    [else
     (for ([n 9]
           #:do [(define ch (parse-column s n))]
           #:unless (char-whitespace? ch))
       (vector-set! stacks n (cons ch (vector-ref stacks n))))]))

(define (parse-stacks in)
  (define stacks (build-vector 9 (λ (x) (list))))
  (define (build in)
    (define r (parse-row (stream-first in) stacks))
    (cond
      [r (build (stream-rest in))]
      [else stacks]))
  (build in))

(define (take-crates stk amount)
  (cond
    [(zero? amount) (values null stk)]
    [else
     (define-values (r stk1) (take-crates (cdr stk) (sub1 amount)))
     (values (cons (car stk) r) stk1)]))

(define-match-expander ->n
  (syntax-parser
    [(->n pat) #'(? string? (app string->number (? number? pat)))]))

(module* part1 #f
  (define (drop-crates stk crates)
    (cond
      [(null? crates) stk]
      [else (drop-crates (cons (car crates) stk) (cdr crates))]))

  (define (move! stacks amount source dest)
    (define src (sub1 source))
    (define dst (sub1 dest))
    (define-values (crates src-stk)
      (take-crates (vector-ref stacks src) amount))
    (define dst-stk (drop-crates (vector-ref stacks dst) crates))
    (vector-set! stacks src src-stk)
    (vector-set! stacks dst dst-stk))

  (define-values (stacks-input insts-input)
    (stream-split input-stream empty-string?))
  (define stacks (parse-stacks stacks-input))

  (for ([s insts-input])
    (match s
      [(regexp #px"move (\\d+) from (\\d+) to (\\d+)"
               (list _ (->n amount) (->n src) (->n dst)))
       (move! stacks amount src dst)]))

  (list->string (for/list ([stk stacks])
                  (car stk))))

(module* part2 #f
  (define (drop-crates stk crates)
    (append crates stk))

  (define (move! stacks amount source dest)
    (define src (sub1 source))
    (define dst (sub1 dest))
    (define-values (crates src-stk)
      (take-crates (vector-ref stacks src) amount))
    (define dst-stk (drop-crates (vector-ref stacks dst) crates))
    (vector-set! stacks src src-stk)
    (vector-set! stacks dst dst-stk))

  (define-values (stacks-input insts-input)
    (stream-split input-stream empty-string?))
  (define stacks (parse-stacks stacks-input))

  (for ([s insts-input])
    (match s
      [(regexp #px"move (\\d+) from (\\d+) to (\\d+)"
               (list _ (->n amount) (->n src) (->n dst)))
       (move! stacks amount src dst)]))

  (list->string (for/list ([stk stacks])
                  (car stk))))
