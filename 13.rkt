#lang at-exp racket

(require advent-of-code
         syntax/parse/define
         threading)

(define-syntax-parse-rule (define-test-input name:id strs:string ...+)
  (define name (open-input-string (~a strs ...))))

@define-test-input[test-input]{
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
}

(define rt (make-readtable #f #\, #\space #f))

(define (read-packet inp)
  (parameterize ([current-readtable rt]) (read inp)))

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 13 #:cache #t)
       #;test-input
       (in-port read-packet)
       sequence->stream))

(define (compare-packets l r)
  (match* {l r}
    [{(? integer?) (? integer?)}
     (cond
       [(= l r) 'same]
       [(< l r) 'right-order]
       [else 'wrong-order])]
    [{'() '()} 'same]
    [{'() _} 'right-order]
    [{_ '()} 'wrong-order]
    [{(? integer?) _} (compare-packets (list l) r)]
    [{_ (? integer?)} (compare-packets l (list r))]
    [{(cons l l*) (cons r r*)}
     (define head-order (compare-packets l r))
     (if (eq? head-order 'same)
         (compare-packets l* r*)
         head-order)]))

(module* part1 #f
  (for/sum ([i (in-naturals 1)]
            [l+r (in-slice 2 input-stream)])
    (match-define (list l r) l+r)
    (if (eq? 'right-order (compare-packets l r)) i 0)))


(module* part2 #f
  (define (sort-packets packets)
    (define (packet< l r) (eq? 'right-order (compare-packets l r)))
    (sort packets packet<))

  (define divider1 (list (list 2)))
  (define divider2 (list (list 6)))
  (define packets (list* divider1 divider2 (stream->list input-stream)))

  (define packets* (sort-packets packets))
  (match-define (list p1 p2)
    (indexes-where packets* (λ (x) (or (equal? x divider1) (equal? x divider2)))))

  (* (add1 p1) (add1 p2)))
