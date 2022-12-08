#lang racket

(require advent-of-code
         threading)

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 8 #:cache #t)
       (in-port read-line)
       sequence->stream))

(define test-input #<<T
30373
25512
65332
33549
35390
T
  )

(define test-stream
  (~>> (open-input-string test-input)
       (in-port read-line)
       sequence->stream))

(define height-info/c
  (or/c #f (cons/dc [l nonnegative-integer?] [r (l) (and/c integer? (>=/c l))])))

(define rank-info/c
  (vector/c height-info/c
            height-info/c
            height-info/c
            height-info/c
            height-info/c
            height-info/c
            height-info/c
            height-info/c
            height-info/c
            height-info/c))

(define size 99)

(define/contract rows (vectorof rank-info/c) (build-vector size (λ (i) (make-vector 10 #f))))
(define/contract columns (vectorof rank-info/c) (build-vector size (λ (i) (make-vector 10 #f))))

(define ZERO (char->integer #\0))
(define (char->height c)
  (- (char->integer c) ZERO))

(define (update-rank! rs nr pos h)
  (define r (vector-ref rs nr))
  (define height-info0 (vector-ref r h))
  (define height-info1
    (match height-info0
      [(cons i j) (cons (min i pos) (max j pos))]
      [#f (cons pos pos)]))
  (vector-set! r h height-info1))

(define (visible-rank? rs nr pos h dir cmp)
  (define r (vector-ref rs nr))
  (not (for/or ([oh (in-range 9 (sub1 h) -1)])
         (match (vector-ref r oh)
           [(? pair? (app dir v)) (cmp pos v)]
           [#f #f]))))

(define (visible? rows columns r c h)
  (or ;; easy cases, the tree is on the edge
      (zero? r) (zero? c)
      (= size (add1 r)) (= size (add1 c))
      ;; otherwise check the collected height information
      (visible-rank? rows r c h car >)
      (visible-rank? rows r c h cdr <)
      (visible-rank? columns c r h car >)
      (visible-rank? columns c r h cdr <)))

(for ([r (in-naturals)]
      [row input-stream])
  (for ([c (in-naturals)]
        [ch (in-string row)])
    (update-rank! rows r c (char->height ch))
    (update-rank! columns c r (char->height ch))))

(for/sum ([r (in-naturals)]
          [row input-stream])
  (for/sum ([c (in-naturals)]
            [ch (in-string row)])
    (if (visible? rows columns r c (char->height ch)) 1 0)))


(module* part1 #f
  )

(module* part2 #f
  )
