#lang racket

(require advent-of-code
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 3 #:cache #t)
       (in-port read-line)
       sequence->stream))

(define (parse-rucksack s)
  (define slen (string-length s))
  (define smid (/ slen 2))
  (define (extract start end)
    (for/set ([ch (in-string s start end)]) ch))
  (values (extract 0 smid) (extract smid slen)))

(define lower-range (sub1 (char->integer #\a)))
(define upper-range (sub1 (char->integer #\A)))

(define (item-priority ch)
  (define chv (char->integer ch))
  (cond
    [(< lower-range chv) (- chv lower-range)]
    [(< upper-range chv) (+ 26 (- chv upper-range))]
    [else
      (error 'item-priorty "cannot assign priority")]))

(module* part1 #f
  (for/sum ([sack input-stream]
            #:do [(define-values (a b) (parse-rucksack sack))]
            [common-item (in-set (set-intersect a b))])
    (item-priority common-item)))

(module* part2 #f
  (define (parse-rucksack2 s)
    (define-values (a b) (parse-rucksack s))
    (set-union a b))

  (for*/sum
   ([sacks (in-slice 3 (stream-map parse-rucksack2 input-stream))]
    [common-item (in-set (apply set-intersect sacks))])
   (item-priority common-item)))

