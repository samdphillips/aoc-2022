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

(define (tail-move rel move)
  (match* {rel move}
    [{rel move} #:when (equal? rel move) 'S]
    [{'S _} 'S]

    [{'U 'D} 'D]
    [{'U (or 'R 'L)} 'S]

    [{'UR (or 'D 'L)} 'DL]
    [{'UR (or 'U 'R)} 'S]

    [{'R 'L} 'L]
    [{'R (or 'U 'D)} 'S]

    [{'DR (or 'U 'L)} 'UL]
    [{'DR (or 'R 'D)} 'S]

    [{'D 'U} 'U]
    [{'D (or 'R 'L)} 'S]

    [{'DL (or 'D 'L)} 'S]
    [{'DL (or 'U 'R)} 'UR]

    [{'L 'R} 'R]
    [{'L (or 'U 'D)} 'S]

    [{'UL (or 'R 'D)} 'DR]
    [{'UL (or 'U 'L)} 'S]))

(define (~rope h t)
  (~a "(" h ", " t ")"))

(module* part1 #f
  (for/fold ([h 0] [t 0] [t-visit (set 0)]
             #:result (set-count t-visit))
            ([inst input-stream]
             #:do [(define h-move (car inst))
                   (define count (cdr inst))]
             [j (in-range count)])
    (define t-rel (offset->dir (- t h)))
    (define t-move (tail-move t-rel h-move))
    (define nh (+ (dir->offset h-move) h))
    (define nt (+ (dir->offset t-move) t))
    ;(displayln (~a (~rope h t) " -> " (~rope nh nt)))
    (values nh nt (set-add t-visit nt))))

(module* part2 #f)

