#lang at-exp racket

(require advent-of-code
         syntax/parse/define
         threading)

(define-syntax-parse-rule (define-test-input name:id strs:string ...+)
  (define name (open-input-string (~a strs ...))))

@define-test-input[test-input]{
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
}

(struct posn (x y) #:transparent)

(define-match-expander ->n
  (syntax-parser
    [(->n pat) #'(? string? (app string->number (? number? pat)))]))

(define (read-sensor inp)
  (match (read-line inp)
    [(regexp #px"Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
             (list _ (->n x0) (->n y0) (->n x1) (->n y1)))
     (list (posn x0 y0) (posn x1 y1))]
    [v v]))

(define (mdistance p0 p1)
  (match-define (posn x0 y0) p0)
  (match-define (posn x1 y1) p1)
  (+ (abs (- x0 x1)) (abs (- y0 y1))))

(define (distance-line p y)
  (abs (- y (posn-y p))))

(struct span (start end) #:transparent)

(define (merge-spans* s0 s*)
  (match-define (span st0 en0) s0)
  (define (inside v) (<= st0 v en0))
  (match s*
    ['() (list s0)]
    [(cons (and s1 (span st1 en1)) s*)
     (match* {(inside st1) (inside en1)}
       [{#f #f} ;; test for adjacency
                (if (inside (sub1 st1))
                    (merge-spans* (span st0 en1) s*)
                    (cons s0 (merge-spans* s1 s*)))]
       [{#f #t} (error 'merge-spans* "unreachable")]
       [{#t #f} (merge-spans* (span st0 en1) s*)]
       [{#t #t} (merge-spans* s0 s*)])]))

(define (merge-spans s*)
  (merge-spans* (car s*) (cdr s*)))

(define (span-size s)
  (- (span-end s) (span-start s)))

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 15 #:cache #t)
       #;test-input
       (in-port read-sensor)
       sequence->stream))

(module* part1 #f
  (define target-row 2000000)

  (define spans
    (for/list ([s+b input-stream]
               #:do [(match-define (list s b) s+b)
                     (define dl (distance-line s target-row))
                     (define db (mdistance s b))]
               #:when (<= dl db))
      (define amt (- db dl))
      (define sx (posn-x s))
      (span (- sx amt) (+ sx amt))))

  (define merged-spans
    (~> (sort spans < #:key span-start)
        merge-spans))

  (for/sum ([s (in-list merged-spans)])
    (span-size s)))

(define (sensor-slice p d dl)
  (define amt (- d dl))
  (define sx (posn-x p))
  (span (- sx amt) (+ sx amt)))

(define (clamp-span lo hi s)
  (span (max lo (span-start s))
        (min hi (span-end s))))

(define limit 4000000)

(define (update-spans1 slices p d)
  (match-define (posn px py) p)
  (for/fold ([slices slices])
            ([x (in-range (- py d) (+ py d))] #:when (<= 0 x limit))
    (define s
      (clamp-span 0 limit (sensor-slice p d (distance-line p x))))
    (hash-update slices x (λ~>> (cons s)) null)))

(define slices
  (for/fold ([slices (hash)]) ([s+b input-stream])
    (match-define (list s b) s+b)
    (update-spans1 slices s (mdistance s b))))

(define merged-slices
  (for/fold ([slices (hash)]) ([(k v) (in-hash slices)])
    (hash-set slices k (~> (sort v < #:key span-start) merge-spans))))
#|
15.rkt> (for/first ([(k v) (in-hash merged-slices)] #:when (not (null? (cdr v)))) (list k v))
(list 2647448 (list (span 0 3131430) (span 3131432 4000000)))
15.rkt> (+ (* limit 3131431) 2647448)
12525726647448
|#

(module* part2 #f)

