#lang racket

(require advent-of-code
         threading)

(define input-bytes
  (~>> (open-aoc-input (find-session) 2022 6 #:cache #t)
       read-bytes-line))

(define (bytes->set bs)
  (for/set ([b (in-bytes bs)]) b))

(define (bytes-marker-chunk bs i)
  (subbytes bs (- i 4) i))

(define (bytes-find-packet-marker bs)
  (for/first ([i (in-range 4 (bytes-length bs))]
              #:do [(define marker (bytes->set (bytes-marker-chunk bs i)))]
              #:when (= 4 (set-count marker)))
    i))

(module* part1 #f
  (bytes-find-packet-marker input-bytes))

(module* part2 #f)

