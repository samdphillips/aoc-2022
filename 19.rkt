#lang racket

(require advent-of-code
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 19 #:cache #t)
       (in-port read-line)
       sequence->stream))

(define (stream-interleave $a $b)
  (cond
    [(stream-empty? $a) $b]
    [else
     (stream-cons (stream-first $a)
                  (stream-interleave $b (stream-rest $a)))]))

(define (stream-bind f $)
  (cond
    [(stream-empty? $) $]
    [else
     (stream-lazy
       (stream-interleave
        (f (stream-first $))
        (stream-bind f (stream-rest $))))]))

(define (stream-dedup $)
  (define (do-dedup seen $)
    (cond
      [(stream-empty? $) $]
      [else
       (define v (stream-first $))
       (if (set-member? seen v)
           (stream-lazy (do-dedup seen (stream-rest $)))
           (stream-cons v (do-dedup (set-add seen v) (stream-rest $))))]))
  (do-dedup (set) $))

(struct state (qo qc qb qg ro rc rb rg) #:transparent)

(define coo 2)
(define cco 3)
(define cbo 3)
(define cbc 8)
(define cgo 3)
(define cgb 12)

(define (can-build-ore-bot? qo qc qb qg)
  (and (<= coo qo)))

(define (can-build-clay-bot? qo qc qb qg)
  (and (<= cco qo)))

(define (can-build-obsidian-bot? qo qc qb qg)
  (and (<= cbo qo) (<= cbc qc)))

(define (can-build-geode-bot? qo qc qb qg)
  (and (<= cgo qo) (<= cgb qb)))

(define (build-ore-bot qo qc qb qg)
  (values (- qo coo) qc qb qg 1 0 0 0))

(define (build-clay-bot qo qc qb qg)
  (values (- qo cco) qc qb qg 0 1 0 0))

(define (build-obsidian-bot qo qc qb qg)
  (values (- qo cbo) (- qc cbc) qb qg 0 0 1 0))

(define (build-geode-bot qo qc qb qg)
  (values (- qo cgo) qc (- qb cgb) qg 0 0 0 1))

(define (no-build? qo qc qb qg) #t)
(define (no-build-bot qo qc qb qg)
  (values qo qc qb qg 0 0 0 0))

(define checks (list
                can-build-geode-bot?
                can-build-obsidian-bot?
                can-build-clay-bot?
                can-build-ore-bot?
                no-build?
                ))

(define builders (list
                  build-geode-bot
                  build-obsidian-bot
                  build-clay-bot
                  build-ore-bot
                  no-build-bot
                  ))

(define (step st)
  (match-define (state qo qc qb qg ro rc rb rg) st)
  (for/stream ([can-build? (in-list checks)]
               [build (in-list builders)]
               #:when (can-build? qo qc qb qg))
    (define-values (qo* qc* qb* qg* do dc db dg) (build qo qc qb qg))
    (state (+ qo* ro) (+ qc* rc) (+ qb* rb) (+ qg* rg)
           (+ ro do) (+ rc dc) (+ rb db) (+ rg dg))))

(define init-state (state 0 0 0 0 1 0 0 0))
(time
 (let ()
   (define results
     (for/fold ([st* (list init-state)]) ([i 24])
       (stream-dedup (stream-bind step st*))))
   (for/fold ([qg 0]) ([r results])
     (max qg (state-qg r)))))

(module* part1 #f)

(module* part2 #f)

