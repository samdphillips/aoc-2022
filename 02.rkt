#lang racket

(require advent-of-code
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 2 #:cache #t)
       (in-port read-line)
       sequence->stream))

(define a-plays '(["A" . rock] ["B" . paper] ["C" . scissors]))

(define (play-score play)
  (length (memq play '(scissors paper rock))))

(define (outcome-score a b)
  (match* {a b}
    [{'rock 'scissors} 0]
    [{'scissors 'paper} 0]
    [{'paper 'rock} 0]
    [{'scissors 'rock} 6]
    [{'paper 'scissors} 6]
    [{'rock 'paper} 6]
    [{_ _} 3]))

(module* part1 #f
  (define b-plays '(["X" . rock] ["Y" . paper] ["Z" . scissors]))

  (define (decode-line s)
    (match-define (regexp #px"^(.).(.)$" (list _ a b)) s)
    (values (dict-ref a-plays a) (dict-ref b-plays b)))

  (for/sum ([r input-stream])
    (define-values (a b) (decode-line r))
    (+ (play-score b) (outcome-score a b))))

(module* part2 #f
  (define results '(["X" . lose] ["Y" . draw] ["Z" . win]))

  (define losing '([rock . scissors] [paper . rock] [scissors . paper]))
  (define winning '([rock . paper] [paper . scissors] [scissors . rock]))

  (define (decode-line s)
    (match-define (regexp #px"^(.).(.)$" (list _ a b)) s)
    (values (dict-ref a-plays a) (dict-ref results b)))

  (define (should-play play result)
    (match result
      ['draw play]
      ['lose (dict-ref losing play)]
      ['win (dict-ref winning play)]))

  (for/sum ([r input-stream])
    (define-values (a result) (decode-line r))
    (define b (should-play a result))
    (+ (play-score b) (outcome-score a b))))

