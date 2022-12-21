#lang at-exp racket

(require advent-of-code
         syntax/parse/define
         threading)

(define-syntax-parse-rule (define-test-input name:id strs:string ...+)
  (define name (open-input-string (~a strs ...))))

@define-test-input[test-input]{
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
}

(struct posn3 (x y z) #:transparent)
(define px posn3-x)
(define py posn3-y)
(define pz posn3-z)

(define (p+ a b)
  (posn3 (+ (px a) (px b))
         (+ (py a) (py b))
         (+ (pz a) (pz b))))

(define rt (make-readtable #f #\, #\space #f))

(define (read-block inp)
  (define ch (peek-char inp))
  (cond
    [(eof-object? ch) ch]
    [else
     (call-with-input-string (read-line inp)
       (λ (inp)
         (parameterize ([current-readtable rt])
           (posn3 (read inp) (read inp) (read inp)))))]))

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 18 #:cache #t)
       #;test-input
       (in-port read-block)
       sequence->stream))

(define (edges p)
  (define-syntax delta
    (syntax-rules ()
      [(_ p acc #f) (acc p)]
      [(_ p acc -1) (let ([v (acc p)]) (cons (sub1 v) v))]
      [(_ p acc +1) (let ([v (acc p)]) (cons v (add1 v)))]))
  (define-syntax-rule (pcopy p dx dy dz)
    (posn3 (delta p px dx) (delta p py dy) (delta p pz dz)))
  (list (pcopy p -1 #f #f)
        (pcopy p +1 #f #f)

        (pcopy p #f -1 #f)
        (pcopy p #f +1 #f)

        (pcopy p #f #f -1)
        (pcopy p #f #f +1)))

(define (find-surfaces blocks)
  (for*/fold ([e* (set)])
             ([b blocks]
              [e (in-list (edges b))])
    (if (set-member? e* e)
        (set-remove e* e)
        (set-add e* e))))

(define (surface-area blocks)
  (set-count (find-surfaces blocks)))

(define (calculate-bounds blocks)
  (define-syntax-rule (mn a b) (if a (min a b) b))
  (define-syntax-rule (mx a b) (if a (max a b) b))
  (for/fold ([ix #f] [jx #f]
             [iy #f] [jy #f]
             [iz #f] [jz #f]
             #:result
             (values (sub1 ix) (add1 jx)
                     (sub1 iy) (add1 jy)
                     (sub1 iz) (add1 jz)))
            ([b blocks])
    (values (mn ix (px b))
            (mx jx (px b))

            (mn iy (py b))
            (mx jy (py b))

            (mn iz (pz b))
            (mx jz (pz b)))))


(define dv* (list (posn3 -1 0 0)
                  (posn3 +1 0 0)
                  (posn3 0 -1 0)
                  (posn3 0 +1 0)
                  (posn3 0 0 -1)
                  (posn3 0 0 +1)))

(define (find-water-blocks oblocks)
  (define-values (ix jx iy jy iz jz) (calculate-bounds oblocks))
  (define (find-water-blocks* hd tl seen)
    (define (dequeue) (values (car hd) (cdr hd)))
    (define (enqueue v tl) (if (set-member? seen v) tl (cons v tl)))
    (define (oob? p)
      (define-values (x y z) (values (px p) (py p) (pz p)))
      (or (< x ix) (> x jx)
          (< y iy) (> y jy)
          (< z iz) (> z jz)))
    (define (enqueue-neighbors w)
      (cond
        [(set-member? seen w) tl]
        [else
         (for/fold ([tl tl]) ([d (in-list dv*)])
           (define w* (p+ w d))
           (cond
             [(oob? w*) tl]
             [(set-member? oblocks w*) tl]
             [else
              (enqueue w* tl)]))]))
    (cond
      [(and (null? hd) (null? tl)) seen]
      [(null? hd) (find-water-blocks* (reverse tl) null seen)]
      [else
       (define-values (w hd*) (dequeue))
       (find-water-blocks* hd*
                           (enqueue-neighbors w)
                           (set-add seen w))]))
  (find-water-blocks* (list (posn3 ix iy iz)) null (set)))

(module* part1 #f
  (surface-area input-stream))

(module* part2 #f
  (define oblocks (for/set ([b input-stream]) b))
  (define wblocks (find-water-blocks oblocks))

  (define osurfaces (find-surfaces oblocks))
  (define wsurfaces (find-surfaces wblocks))
  (set-count (set-intersect osurfaces wsurfaces)))
