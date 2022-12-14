#lang at-exp racket

(require advent-of-code
         syntax/parse/define
         threading)

(define-syntax-parse-rule (define-test-input name:id strs:string ...+)
  (define name (open-input-string (~a strs ...))))

@define-test-input[test-input]{
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
}

(define-match-expander ->n
  (syntax-parser
    [(->n pat) #'(? string? (app string->number (? number? pat)))]))

(define-match-expander complex
  (syntax-parser
    [(complex real-pat imag-pat)
     #'(? number? (and (app real-part real-pat) (app imag-part imag-pat)))]))

(define (skip-arrows inp) (read inp))

(define (skip-spaces inp)
  (define ch (peek-char inp))
  (when (and (char? ch) (char-whitespace? ch))
    (read-char inp)
    (skip-spaces inp)))

(define rt (make-readtable #f #\, #\space #f))

(define (read-coord inp)
  (parameterize ([current-readtable rt])
    (define r (read inp))
    (define i (read inp))
    (make-rectangular r i)))

(define (read-rock-path inp)
  (skip-spaces inp)
  (define (read-tail)
    (cond
      [(eof-object? (peek-char inp)) null]
      [(char=? #\newline (peek-char inp)) null]
      [else
       (skip-arrows inp)
       (cons (read-coord inp) (read-tail))]))
  (cond
    [(eof-object? (peek-char inp)) eof]
    [else
     (cons (read-coord inp) (read-tail))]))

(define input-stream
  (~>> #;(open-aoc-input (find-session) 2022 14 #:cache #t)
       test-input
       (in-port read-rock-path)
       sequence->stream))

(define (step-between p0 p1)
  (match (- p1 p0)
    [(complex 0 v) (make-rectangular 0 (sgn v))]
    [(complex v 0) (make-rectangular (sgn v) 0)]))

(define (in-crange p0 p1 step)
  (make-do-sequence
   (λ ()
     (values values
             (λ (p) (+ p step))
             p0
             #f
             #f
             (λ (p v) (not (= p p1)))))))

(define (trace-rock-segment env p0 p1)
  (define step (step-between p0 p1))
  (for/fold ([env env]) ([p (in-crange p0 p1 step)])
    (hash-set env p 'rock)))

(define (trace-rock-path* env p0 p*)
  (cond
    [(null? p*) env]
    [else
     (define p1 (car p*))
     (define env1 (trace-rock-segment env p0 p1))
     (trace-rock-path* env1 p1 (cdr p*))]))

(define (trace-rock-path env p*)
  (trace-rock-path* env (car p*) (cdr p*)))

(define d   0+1i)
(define dl -1+1i)
(define dr +1+1i)
(define init-m* (list d dl dr))

(define (next-move p env limit)
  (define (step p m*)
    (cond
      [(null? m*) (if (= 500 p) #f p)]
      [else
       (define p* (+ p (car m*)))
       (cond
         [(= limit (imag-part p*)) (step p (cdr m*))]
         [(hash-has-key? env p*) (step p (cdr m*))]
         [else (step p* init-m*)])]))
  (step p init-m*))

(define env
  (for/fold ([env (hash)]) ([p* input-stream])
    (trace-rock-path env p*)))

(define limit
  (for/fold ([m #f] #:result (+ 2 m))
            ([v (in-hash-keys env)])
    (if m (max m (imag-part v)) (imag-part v))))

(for/fold ([env env] [i 0] #:result i)
          ([i (in-naturals 1)]
           #:do [(define p (next-move 500 env limit))]
           #:break (not p))
  (values (hash-set env p 'sand) i))

(module* part1 #f)

(module* part2 #f)

