#lang racket

(require advent-of-code
         minikanren
         threading)

(define test-data #<<TEST
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
TEST
  )

(define (read-instruction inp)
  (define (with-input-from-string* s thunk)
    (cond
      [(eof-object? s) s]
      [else
       (with-input-from-string s thunk)]))
  (with-input-from-string* (read-line inp)
    (λ () (cons (read) (read)))))

(define test? #f)

(define input-stream
  (~>> (if test?
           (open-input-string test-data)
           (open-aoc-input (find-session) 2022 9 #:cache #t))
       (in-port read-instruction)
       sequence->stream))

(defrel (dir-offset dir offset)
  (conde
   [(== dir 'S)  (== offset  0)]
   [(== dir 'U)  (== offset  0+1i)]
   [(== dir 'UR) (== offset  1+1i)]
   [(== dir 'R)  (== offset  1+0i)]
   [(== dir 'DR) (== offset  1-1i)]
   [(== dir 'D)  (== offset  0-1i)]
   [(== dir 'DL) (== offset -1-1i)]
   [(== dir 'L)  (== offset -1+0i)]
   [(== dir 'UL) (== offset -1+1i)]))

(define (dir->offset dir)
  (car (run 1 (offset) (dir-offset dir offset))))
(define (offset->dir offset)
  (car (run 1 (dir) (dir-offset dir offset))))

(define (tail-move h t move)
  (define rel (offset->dir (- t h)))
  (match* {rel move}
    [{rel move} #:when (equal? rel move) 'S]
    [{'S _} 'S]
    [{_ 'S} 'S]

    [{'U (or 'UR 'R 'L 'UL)} 'S]
    [{'U d} d]

    [{'UR 'DR} 'D]
    [{'UR 'UL} 'L]
    [{'UR (or 'D 'DL 'L)} 'DL]
    [{'UR (or 'U 'R)} 'S]

    [{'R (or 'U 'UR 'DR 'D)} 'S]
    [{'R d} d]

    [{'DR 'UR} 'U]
    [{'DR 'DL} 'L]
    [{'DR (or 'U 'UL 'L)} 'UL]
    [{'DR (or 'R 'D)} 'S]

    [{'D (or 'R 'DR 'DL 'L)} 'S]
    [{'D d} d]

    [{'DL 'DR} 'R]
    [{'DL 'UL} 'U]
    [{'DL (or 'U 'UR 'R)} 'UR]
    [{'DL (or 'D 'L)} 'S]

    [{'L (or 'U 'D 'DL 'UL)} 'S]
    [{'L d} d]

    [{'UL 'UR} 'R]
    [{'UL 'DL} 'D]
    [{'UL (or 'R 'DR 'D)} 'DR]
    [{'UL (or 'U 'L)} 'S]))

(define (~rope h t)
  (~a "(" h ", " t ")"))

(define (calculate-rope-moves rs move)
  (match rs
    [(cons h (and (cons t _) rs))
     (cons move (calculate-rope-moves rs (tail-move h t move)))]
    [_ (list move)]))

(define (rope-move rs ms)
  (for/list ([r (in-list rs)] [m (in-list ms)])
    (+ r (dir->offset m))))

(module* part1 #f
  (for/fold ([h 0] [t 0] [t-visit (set 0)]
             #:result (set-count t-visit))
            ([inst input-stream]
             #:do [(define h-move (car inst))
                   (define count (cdr inst))]
             [j (in-range count)])
    (define t-move (tail-move h t h-move))
    (define nh (+ (dir->offset h-move) h))
    (define nt (+ (dir->offset t-move) t))
    ;(displayln (~a (~rope h t) " -> " (~rope nh nt)))
    (values nh nt (set-add t-visit nt))))

(module* part2 #f
  (for/fold ([r (make-list 10 0)] [t-visit (set 0)]
             #:result (set-count t-visit))
            ([inst input-stream]
             #:do [(define move (car inst))
                   (define count (cdr inst))]
             [j (in-range count)])
    (define nr (rope-move r (calculate-rope-moves r move)))
    (define t (list-ref nr 9))
    (values nr (set-add t-visit t))))
