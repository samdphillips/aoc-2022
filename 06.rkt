#lang racket

(require advent-of-code
         threading)

(define input-bytes
  (~>> (open-aoc-input (find-session) 2022 6 #:cache #t)
       read-bytes-line))

(define (bytes->set bs)
  (for/set ([b (in-bytes bs)]) b))

(define ((make-bytes-find-marker size) bs)
  (define (marker-chunk i)
    (subbytes bs (- i size) i))
  (for/first ([i (in-range size (bytes-length bs))]
              #:do [(define marker (bytes->set (marker-chunk i)))]
              #:when (= size (set-count marker)))
    i))

(module* part1 #f
  (define bytes-find-packet-marker (make-bytes-find-marker 4))
  (bytes-find-packet-marker input-bytes))

(module* part2 #f
  (define bytes-find-message-marker (make-bytes-find-marker 14))
  (bytes-find-message-marker input-bytes))
