#lang at-exp racket

(require advent-of-code
         syntax/parse/define
         threading)

(define-syntax-parse-rule (define-test-input name:id strs:string ...+)
  (define name (open-input-string (~a strs ...))))

@define-test-input[test-input]{
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
}

(struct posn3 (x y z) #:transparent)
(define px posn3-x)
(define py posn3-y)
(define pz posn3-z)

(define rt (make-readtable #f #\, #\space #f))

(define (read-block inp)
  (define ch (peek-char inp))
  (cond
    [(eof-object? ch) ch]
    [else
     (call-with-input-string (read-line inp)
       (λ (inp)
         (parameterize ([current-readtable rt])
           (posn3 (read inp) (read inp) (read inp)))))]))

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 18 #:cache #t)
       #;test-input
       (in-port read-block)
       sequence->stream))

(define (edges p)
  (define-syntax delta
    (syntax-rules ()
      [(_ p acc #f) (acc p)]
      [(_ p acc -1) (let ([v (acc p)]) (cons (sub1 v) v))]
      [(_ p acc +1) (let ([v (acc p)]) (cons v (add1 v)))]))
  (define-syntax-rule (pcopy p dx dy dz)
    (posn3 (delta p px dx) (delta p py dy) (delta p pz dz)))
  (list (pcopy p -1 #f #f)
        (pcopy p +1 #f #f)

        (pcopy p #f -1 #f)
        (pcopy p #f +1 #f)

        (pcopy p #f #f -1)
        (pcopy p #f #f +1)))

(define (surface-area blocks)
  (for*/fold ([e* (set)] #:result (set-count e*))
             ([b blocks]
              [e (in-list (edges b))])
    (if (set-member? e* e)
        (set-remove e* e)
        (set-add e* e))))

(module* part1 #f
  (surface-area input-stream))

(module* part2 #f)

