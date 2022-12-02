#lang racket

(require advent-of-code
         minikanren
         syntax/parse/define
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 2 #:cache #t)
       (in-port read-line)
       sequence->stream))

(define l '(rock paper scissors))
(define w '(paper scissors rock))

(defrel (caro c a) (fresh (d) (== c (cons a d))))
(defrel (cdro c d) (fresh (a) (== c (cons a d))))

(defrel (lookup them us a b)
  (conde
   [(fresh (a0 b0)
      (caro a a0) (caro b b0)
      (== a0 them) (== b0 us))]
   [(fresh (a* b*)
      (cdro a a*) (cdro b b*)
      (lookup them us a* b*))]))

(defrel (roshambo them us outcome)
  (conde
   [(== them us) (== outcome 'draw)]
   [(== outcome 'win) (lookup them us l w)]
   [(== outcome 'lose) (lookup them us w l)]))

(defrel (play-score play outcome score)
  (fresh (pscore oscore)
    (conde
     [(== play 'rock) (== pscore 1)]
     [(== play 'paper) (== pscore 2)]
     [(== play 'scissors) (== pscore 3)])
    (conde
     [(== outcome 'win) (== oscore 6)]
     [(== outcome 'draw) (== oscore 3)]
     [(== outcome 'lose) (== oscore 0)])
    (project (pscore oscore)
      (== score (+ pscore oscore)))))

(define them-plays '(["A" . rock] ["B" . paper] ["C" . scissors]))

(define-syntax-parse-rule (runv (x:id ...) g ...)
  (apply values (run 1 (x ...) g ...)))

(module* part1 #f
  (define us-plays '(["X" . rock] ["Y" . paper] ["Z" . scissors]))

  (define (decode-line s)
    (match-define (regexp #px"^(.).(.)$" (list _ a b)) s)
    (values (dict-ref them-plays a) (dict-ref us-plays b)))

  (for/sum ([r input-stream])
    (define-values (them us) (decode-line r))
    (runv (score)
      (fresh (outcome)
        (roshambo them us outcome)
        (play-score us outcome score)))))

(module* part2 #f
  (define results '(["X" . lose] ["Y" . draw] ["Z" . win]))

  (define (decode-line s)
    (match-define (regexp #px"^(.).(.)$" (list _ a b)) s)
    (values (dict-ref them-plays a) (dict-ref results b)))

  (for/sum ([r input-stream])
    (define-values (them outcome) (decode-line r))
    (runv (score)
      (fresh (us)
        (roshambo them us outcome)
        (play-score us outcome score)))))

