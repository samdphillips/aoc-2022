#lang racket

(require advent-of-code
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 X #:cache #t)
       (in-port read-line)
       sequence->stream))

(module* part1 #f)

(module* part2 #f)

