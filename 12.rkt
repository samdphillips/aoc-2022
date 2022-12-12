#lang at-exp racket

(require advent-of-code
         graph
         syntax/parse/define
         threading)

(define-syntax-parse-rule (define-test-input name:id strs:string ...+)
  (define name (open-input-string (~a strs ...))))

@define-test-input[test-input]{
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
}

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 12 #:cache #t)
       (in-port read-bytes-line)
       sequence->stream))

(define (bytes-find bs t)
  (for/first ([i (in-naturals)]
              [b (in-bytes bs)]
              #:when (= t b))
    i))

(define S-byte (char->integer #\S))
(define E-byte (char->integer #\E))
(define a-byte (char->integer #\a))
(define z-byte (char->integer #\z))

(define neighbors '(-1i +1 +1i -1))

(struct heightmap (start end width height bytes)
  #:transparent
  #:methods gen:graph
  [(define (in-vertices g)
     (for*/stream ([y (in-range (heightmap-height g))]
                   [x (in-range (heightmap-width g))])
       (make-rectangular x y)))
   (define (in-neighbors g v1)
     (for/stream ([d (in-list neighbors)]
                  #:do [(define v2 (+ d v1))]
                  #:when (and (heightmap-in-bounds? g v2)
                              (heightmap-connected? g v1 v2)))
       v2))])

(define (heightmap-in-bounds? hm v)
  (define r (real-part v))
  (define i (imag-part v))
  (and (<= 0 r) (< r (heightmap-width hm))
       (<= 0 i) (< i (heightmap-height hm))))

(define heightmap-direction (make-parameter 'ascend))

(define (heightmap-connected? hm v1 v2)
  (define (pos->offset z)
    (+ (real-part z) (* (imag-part z) (heightmap-width hm))))
  (define p1 (pos->offset v1))
  (define p2 (pos->offset v2))
  (define b (heightmap-bytes hm))
  (match (heightmap-direction)
    ['ascend (<= (- (bytes-ref b p2) (bytes-ref b p1)) 1)]
    ['descend (<= (- (bytes-ref b p1) (bytes-ref b p2)) 1)]))

(define (make-heightmap input-stream)
  (define start #f)
  (define end #f)
  (define buf (open-output-bytes))
  (define width (bytes-length (stream-first input-stream)))
  (for ([line input-stream]
        [row (in-naturals)])
    (cond
      [(bytes-find line S-byte)
       => (λ (col)
            (set! start (make-rectangular col row))
            (bytes-set! line col a-byte))])
    (cond
      [(bytes-find line E-byte)
       => (λ (col)
            (set! end (make-rectangular col row))
            (bytes-set! line col z-byte))])
    (write-bytes line buf))
  (define bs (get-output-bytes buf))
  (heightmap start
             end
             width
             (/ (bytes-length bs) width)
             bs))

(define m (make-heightmap input-stream))

(define-values (distances traces)
  (parameterize ([heightmap-direction 'descend])
    (dijkstra m (heightmap-end m))))

(module* part1 #f
  (define-values (distances traces)
    (dijkstra m (heightmap-start m)))
  (hash-ref distances (heightmap-end m) #f))

(module* part2 #f
  (define a-distances
    (for/list ([b (in-bytes (heightmap-bytes m))]
               [j (in-naturals)]
               #:when (= b a-byte)
               #:do
               [(define p
                  (let-values ([(i r) (quotient/remainder j (heightmap-width m))])
                    (make-rectangular r i)))
                (define d (hash-ref distances p #f))]
               #:when d)
      (cons p d)))
  (car (sort a-distances < #:key cdr)))
