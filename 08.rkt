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

(define minmax-height-info/c
  (or/c #f
        (cons/dc [l nonnegative-integer?]
                 [r (l) (and/c integer? (>=/c l))])))

(define height-info/c
  (or/c null?
        (cons/dc [hd nonnegative-integer?]
                 [tl (hd) (or/c null? (λ (tl) (< (car tl) hd)))])))

(define (rank-info/c v)
  (vector/c v v v v v v v v v v))

(define ZERO (char->integer #\0))
(define (char->height c)
  (- (char->integer c) ZERO))

(module* part1 #f
  (define size 99)

  (define/contract rows
    (vectorof (rank-info/c minmax-height-info/c))
    (build-vector size (λ (i) (make-vector 10 #f))))
  (define/contract columns
    (vectorof (rank-info/c minmax-height-info/c))
    (build-vector size (λ (i) (make-vector 10 #f))))

  (define (update-rank! rs nr pos h)
    (define r (vector-ref rs nr))
    (define height-info
      (match (vector-ref r h)
        [(cons i j) (cons (min i pos) (max j pos))]
        [#f (cons pos pos)]))
    (vector-set! r h height-info))

  (for ([r (in-naturals)] [row input-stream])
    (for ([c (in-naturals)] [ch (in-string row)])
      (update-rank! rows r c (char->height ch))
      (update-rank! columns c r (char->height ch))))

  (define (visible-rank? rs nr pos h dir cmp)
    (define r (vector-ref rs nr))
    (not (for/or ([oh (in-range 9 (sub1 h) -1)])
           (match (vector-ref r oh)
             [(? pair? (app dir v)) (cmp pos v)]
             [#f #f]))))

  (define (visible? rows columns r c h)
    (or ;; easy cases, the tree is on the edge
        (zero? r)
        (zero? c)
        (= size (add1 r))
        (= size (add1 c))
        ;; otherwise check the collected height information
        (visible-rank? rows r c h car >)
        (visible-rank? rows r c h cdr <)
        (visible-rank? columns c r h car >)
        (visible-rank? columns c r h cdr <)))

  (for/sum ([r (in-naturals)] [row input-stream])
   (for/sum ([c (in-naturals)] [ch (in-string row)])
     (if (visible? rows columns r c (char->height ch)) 1 0))))

(module* part2 #f
  (define size 99)

  (define/contract rows
    (vectorof (rank-info/c height-info/c))
    (build-vector size (λ (i) (make-vector 10 null))))
  (define/contract columns
    (vectorof (rank-info/c height-info/c))
    (build-vector size (λ (i) (make-vector 10 null))))

  (define (update-rank! rs nr pos h)
    (define r (vector-ref rs nr))
    (define height-info (set-add (vector-ref r h) pos))
    (vector-set! r h height-info))

  (for ([r (in-naturals)] [row input-stream])
    (for ([c (in-naturals)] [ch (in-string row)])
      (update-rank! rows r c (char->height ch))
      (update-rank! columns c r (char->height ch))))

  (define (trees-visible-rank rs nr pos h select combine dist)
    (define r (vector-ref rs nr))
    (for/fold ([n #f] #:result (dist pos n)) ([oh (in-range h 10)])
      (match (vector-ref r oh)
        [(list (? (λ (v) (> v pos)) bigger) ... smaller ...)
         (let ([smaller (cond
                          [(null? smaller) smaller]
                          [(= pos (car smaller)) (cdr smaller)]
                          [else smaller])])
           (combine n (select bigger smaller)))]
        [(list) n])))

  (define (select-top top bot)
    top)
  (define (select-bot top bot)
    bot)

  (define ((combine-op op) v vs)
    (match* {v vs}
      [{v (list)} v]
      [{#f (list v)} v]
      [{#f vs} (apply op vs)]
      [{v vs} (op v (apply op vs))]))

  (define (dist-top p1 p2)
    (if p2 (- p2 p1) (- size 1 p1)))

  (define (dist-bot p1 p2)
    (if p2 (- p1 p2) p1))

  (define (trees-visible-score rows columns r c h)
    (* (trees-visible-rank rows
                           r
                           c
                           h
                           select-bot
                           (combine-op max)
                           dist-bot)
       (trees-visible-rank rows
                           r
                           c
                           h
                           select-top
                           (combine-op min)
                           dist-top)
       (trees-visible-rank columns
                           c
                           r
                           h
                           select-bot
                           (combine-op max)
                           dist-bot)
       (trees-visible-rank columns
                           c
                           r
                           h
                           select-top
                           (combine-op min)
                           dist-top)))

  (for/fold ([s #f]) ([r (in-naturals)] [row input-stream])
    (for/fold ([sc s]) ([c (in-naturals)] [ch (in-string row)])
      (define ts
        (trees-visible-score rows columns r c (char->height ch)))
      (if sc (max sc ts) ts))))
