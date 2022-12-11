#lang at-exp racket

(require advent-of-code
         syntax/parse/define
         threading)

(define-syntax-parse-rule (define-test name:id strs ...+)
  (define name (open-input-string (~a strs ...))))

@define-test[test-input]{
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
}

(struct monkey (id items proc) #:transparent)

(define-match-expander ->atom
  (syntax-parser
    [(->atom pat)
     #'(? string? (app (λ (s) (with-input-from-string s read)) pat))]))

(define (make-operation rator rand0 rand1)
  (define (randf v)
    (cond
      [(number? v) (λ (o) v)]
      [(symbol? v) (λ (o) o)]))
  (define ratorf
    (match rator
      ['* *]
      ['+ +]))
  (define rand0f (randf rand0))
  (define rand1f (randf rand1))
  (lambda (o)
    (ratorf (rand0f o) (rand1f o))))

(define (make-monkey id items rator rand0 rand1 div-test if-true if-false)
  (define operation (make-operation rator rand0 rand1))
  (monkey id
          items
          (lambda (o monkeys)
            (define n (quotient (operation o) 3))
            (monkey-update-items! monkeys
                                  (if (zero? (remainder n div-test))
                                      if-true
                                      if-false)
                                  n))))

(define (monkey-update-items! monkeys i item)
  (define m (vector-ref monkeys i))
  (vector-set! monkeys i
               (struct-copy monkey m
                            [items (append (monkey-items m) (list item))])))

(define (process-monkey monkeys scores i)
  (define m (vector-ref monkeys i))
  (define proc (monkey-proc m))
  (define items (monkey-items m))
  (vector-set! scores i (+ (length items) (vector-ref scores i)))
  (for ([i (in-list items)])
    (proc i monkeys))
  (vector-set! monkeys i (struct-copy monkey m [items null])))

(define (process-round monkeys scores)
  (for ([i (in-range (vector-length monkeys))])
    (process-monkey monkeys scores i)))

(define (read-monkey inp)
  (define-syntax-parse-rule (parse rx-pat pats ...)
    (match-define (regexp rx-pat (list _ pats ...)) (read-line inp)))
  (cond
    [(eof-object? (peek-byte inp)) eof]
    [else
      (parse #px"^Monkey (\\d+):" (->atom id))
      (parse #px"^  Starting items: ((\\d+)(, (\\d+))*)"
             (app (λ (m) (string-split m #px", "))
                  (list (->atom items) ...))
             _  ...)
      (parse #px"^  Operation: new = (\\w+) (.) (\\w+)"
             (->atom rand0) (->atom rator) (->atom rand1))
      (parse #px"^  Test: divisible by (\\d+)" (->atom div-test))
      (parse #px"^    If true: throw to monkey (\\d+)" (->atom if-true))
      (parse #px"^    If false: throw to monkey (\\d+)" (->atom if-false))
      (read-line inp)
      (make-monkey id items rator rand0 rand1 div-test if-true if-false)]))

(define init-stream
  (~>> (open-aoc-input (find-session) 2022 11 #:cache #t)
       #;test-input
       (in-port read-monkey)))

(define monkeys (for/vector ([m init-stream]) m))
(define scores (make-vector (vector-length monkeys) 0))

(for ([x 20])
  (process-round monkeys scores))

(module* part1 #f)

(module* part2 #f)

