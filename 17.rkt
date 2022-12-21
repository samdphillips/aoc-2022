#lang racket

(require (for-syntax racket/syntax
                     syntax/parse)
         advent-of-code
         syntax/parse/define
         threading)

(struct posn (x y) #:transparent)
(define px posn-x)
(define py posn-y)

(define (p+ a b)
  (posn (+ (px a) (px b)) (+ (py a) (py b))))

(struct rock (shape) #:transparent)

(define-syntax-parse-rule (define-rock-shape name:id (x:nat y:nat) ...)
  #:with maker (format-id #'name "make-~a" #'name)
  (define (maker floor)
    (rock (set (posn (+ x 2) (+ y 4 floor)) ...))))

(define-rock-shape hline
  (0 0) (1 0) (2 0) (3 0))

(define-rock-shape plus
  (0 1) (1 0) (1 1) (1 2) (2 1))

(define-rock-shape ell
  (0 0) (1 0) (2 0) (2 1) (2 2))

(define-rock-shape vline
  (0 0) (0 1) (0 2) (0 3))

(define-rock-shape square
  (0 0) (0 1) (1 0) (1 1))

(define (posn-collision? heap p)
  (or (set-member? heap p) (< (px p) 0) (> (px p) 6)))

(define (rock-collision? heap rck d)
  (for/or ([p (in-set (rock-shape rck))])
    (posn-collision? heap (p+ p d))))

(define (rock-move rck d)
  (rock (for/set ([p (in-set (rock-shape rck))]) (p+ p d))))

(define init-heap
  (set (posn 0 0)
       (posn 1 0)
       (posn 2 0)
       (posn 3 0)
       (posn 4 0)
       (posn 5 0)
       (posn 6 0)))

(define test-string
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(define (infinite-stream s)
  (sequence->stream (in-cycle s)))

(define (load-jets s)
  (infinite-stream
   (~>> (in-string s)
        sequence->list
        (filter-not (λ~> char-whitespace?))
        (map (match-lambda [#\< (posn -1 0)] [#\> (posn 1 0)])))))

(define jets (load-jets test-string))

#;
(define jets
  (~> (open-aoc-input (find-session) 2022 17 #:cache #t)
      port->string
      load-jets))

(define rocks
  (infinite-stream (list make-hline make-plus make-ell make-vline make-square)))

(define down (posn 0 -1))

(define (simulate rocks jets heap heap-top nrocks)
  (define (update-heap-top r)
    (for/fold ([m heap-top]) ([p (in-set (rock-shape r))])
      (max m (py p))))
  (define (update-heap r jets)
    (simulate (stream-rest rocks)
              jets
              (set-union heap (rock-shape r))
              (update-heap-top r)
              (sub1 nrocks)))
  (define (simulate-rock r0 jets)
    (define d (stream-first jets))
    (define r1 (if (rock-collision? heap r0 d) r0 (rock-move r0 d)))
    (if (rock-collision? heap r1 down)
        (update-heap r1 (stream-rest jets))
        (simulate-rock (rock-move r1 down) (stream-rest jets))))
  (define (next-rock)
    ((stream-first rocks) heap-top))
  (cond
    [(zero? nrocks) (values rocks jets heap heap-top)]
    [else
     #;(when (zero? (modulo nrocks 1000))
       (displayln nrocks))
     (define r (next-rock))
     (simulate-rock r jets)]))

(define nd (list (posn -1 0) (posn 1 0) (posn 0 -1)))

(define (heap-split-at* hd tl seen min-y heap)
  #;(pretty-write `(heap-split-at* ,hd ,tl ,seen ,min-y))
  (define (dequeue) (values (car hd) (cdr hd)))
  (define (enqueue v tl) (if (set-member? seen v) tl (cons v tl)))
  (define (enqueue-neighbors w)
    (cond
      [(set-member? seen w) tl]
      [else
        (for/fold ([tl tl]) ([n (in-list nd)])
          (define w* (p+ w n))
          (cond
            [(posn-collision? heap w*) tl]
            [else (enqueue w* tl)]))]))
  (cond
    [(and (null? hd) (null? tl)) (sub1 min-y)]
    [(null? hd) (heap-split-at* (reverse tl) null seen min-y heap)]
    [else
     (define-values (w hd*) (dequeue))
     (heap-split-at* hd*
                     (enqueue-neighbors w)
                     (set-add seen w)
                     (min (py w) min-y)
                     heap)]))

(define (prune-heap heap heap-bottom)
  (define d (posn 0 (- heap-bottom)))
  (for/set ([v (in-set heap)]
            #:unless (< (py v) heap-bottom))
    (p+ v d)))

(define (heap-split-at heap heap-top)
  (heap-split-at* (list (posn 0 (add1 heap-top))) null (set) (add1 heap-top) heap))

(define (simulate/gc rocks jets heap heap-top heap-bottom nrocks crocks)
  #;(pretty-write (list heap-top heap-bottom nrocks))
  (cond
    [(zero? nrocks) (+ heap-top heap-bottom)]
    [else
     (define heap-bottom*
       (heap-split-at heap heap-top))
     (define heap* (prune-heap heap heap-bottom*))
     (define heap-top* (- heap-top heap-bottom*))
     (define-values (rocks* jets* heap** heap-top**)
       (simulate rocks jets heap* heap-top* crocks))
     (define nrocks* (- nrocks crocks))
     (simulate/gc rocks*
                  jets*
                  heap**
                  heap-top**
                  (+ heap-bottom* heap-bottom)
                  nrocks*
                  (min crocks nrocks*))]))

#;(time (simulate/gc rocks jets init-heap 0 0 1000000000000 1000))
(time (simulate/gc rocks jets init-heap 0 0 500000 1000))

(module* part1 #f
  (define-values (rocks* jets* heap heap-top)
    (simulate rocks jets init-heap 0 2022))
  heap-top)

(module* part2 #f
  (define-values (rocks* jets* heap heap-top)
    (time
     (simulate rocks jets init-heap 0 50000 #;1000000000000)))
  heap-top)
