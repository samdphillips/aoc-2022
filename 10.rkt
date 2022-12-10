#lang at-exp racket

(require advent-of-code
         racket/generator
         syntax/parse/define
         threading)

(define (read-instruction inp)
  (define s (read-line inp))
  (cond
    [(eof-object? s) eof]
    [else (with-input-from-string s port->list)]))

(define-for-syntax debug? #f)

(define-syntax debugln
  (if debug?
      (make-rename-transformer #'displayln)
      (λ (stx) #'(begin))))

(define-syntax-parse-rule (define-test name:id strs ...+)
  (define name (open-input-string (~a strs ...))))

@define-test[t1]{
noop
addx 3
addx -5
}

@define-test[t2]{
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
}

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 10 #:cache #t)
       (in-port read-instruction)
       sequence->stream))

(define (~inst pc inst rval0 rval1)
  (~a "[" pc "] " inst " x: " rval0 " -> " rval1))

(define (run-program input-stream trace)
  (for/fold ([pc 1] [x 1]) ([inst input-stream])
    (match inst
      [(list 'noop)
       (debugln (~inst pc inst x x))
       (trace pc x)
       (values (add1 pc) x)]
      [(list 'addx n)
       (debugln (~inst pc inst x x))
       (trace pc x)
       (debugln (~inst (add1 pc) inst x (+ n x)))
       (trace (add1 pc) x)
       (values (+ 2 pc) (+ n x))])))

(define program-trace
  (in-producer
    (generator () (run-program input-stream yield) (yield #f #f))
    (λ (pc x) (not pc))))

(module* part1 #f
  (define (check-cycle? pc)
    (zero? (modulo (- pc 20) 40)))

  (for/fold ([signal 0]) ([(pc x) program-trace]
                          #:when (check-cycle? pc))
    (displayln (~a pc " " x " -> " (* pc x)))
    (+ signal (* pc x))))

(module* part2 #f
  (define (make-display)
    (build-vector 6 (λ (i) (make-string 40 #\?))))

  (define (display-set! dsp x y ch)
    (string-set! (vector-ref dsp y) x ch))

  (define (inside-sprite? px x)
    (<= -1 (- px x) 1))

  (define dsp (make-display))
  (for ([(clk x) program-trace])
    (define-values (py px)
      (quotient/remainder (sub1 clk) 40))
    (display-set! dsp px py (if (inside-sprite? px x) #\# #\.)))
  (for ([row dsp])
    (displayln row)))
