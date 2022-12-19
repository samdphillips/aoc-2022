#lang at-exp racket

(require advent-of-code
         graph
         syntax/parse/define
         threading)

(define-syntax-parse-rule (define-test-input name:id strs:string ...+)
  (define name (open-input-string (~a strs ...))))

@define-test-input[test-input]{
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
}

(define-match-expander ->n
  (syntax-parser
    [(->n pat) #'(? string? (app string->number (? number? pat)))]))

(define-match-expander ->s
  (syntax-parser
    [(->s pat) #'(? string? (app string->symbol pat))]))

(define-match-expander split
  (syntax-parser
    [(split pats ...) #'(? string? (app (lambda~> (string-split ", ")) (list pats ...)))]))

(define (read-node inp)
  (match (read-line inp)
    [(regexp #px"Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)"
             (list _ (->s name) (->n flow) (split (->s n*) ...)))
     (list name flow n*)]
    [v v]))

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 16 #:cache #t)
       #;test-input
       (in-port read-node)
       sequence->stream))

(define-values (weights cave-graph)
  (let ()
    (define-values (w n)
      (for/fold ([w (hash)] [n (hash)]) ([v input-stream])
        (match-define (list name flow n*) v)
        (values (hash-set w name flow)
                (hash-set n name n*))))
    (values w (weighted-graph/undirected
               (for*/list ([(u v*) (in-hash n)]
                            [v (in-list v*)])
                 (list 1 u v))))))

(define (write-dot-file filename graph vertex-label)
  (call-with-output-file filename
    #:exists 'replace
    (λ (outp)
      (graphviz graph
                #:output outp
                #:vertex-attributes
                (list (list 'label vertex-label))))))

(define (vertex-label v)
  (~a v "\n" (hash-ref weights v)))

(define (remove-zero-flow-vertices! g start flows)
  (for ([(u w) (in-hash flows)]
        #:unless (eq? start u)
        #:when (zero? w))
    (define collapse-edges
      (for/list ([v (in-neighbors g u)])
        (list v (edge-weight g u v))))

    (for* ([u* (in-list collapse-edges)]
           [v* (in-list collapse-edges)]
           #:do
           [(match-define (list uv uw) u*)
            (match-define (list vv vw) v*)
            (define nw (+ uw vw))]
           #:unless
           (eq? uv vv))
      (cond
        [(has-edge? g uv vv)
         (when (< nw (edge-weight g uv vv))
           (remove-edge! g uv vv)
           #;(displayln (~a u " :: " uv " -> " vv))
           (add-edge! g uv vv nw))]
        [else
         #;(displayln (~a u " :: " uv " -> " vv))
         (add-edge! g uv vv nw)]))
    (remove-vertex! g u)))

#;(write-dot-file "16t0.dot" cave-graph vertex-label)
(remove-zero-flow-vertices! cave-graph 'AA weights)
#;(write-dot-file "16t1.dot" cave-graph vertex-label)

(define distance
  (let ([dt (floyd-warshall cave-graph)])
    (λ (u v)
      (define d (hash-ref dt (list u v) #f))
      (and d (inexact->exact d)))))

(define (trim-path p t)
  (define (trim-path* u v p t)
    (define dt (add1 (distance u v)))
    (cond
      [(< t dt) null]
      [else
       (define t* (- t dt))
       (define flow (hash-ref weights v))
       (define step (list t* flow v))
       (cond
         [(null? p) (list step)]
         [else
          (cons step (trim-path* v (car p) (cdr p) t*))])]))
  (trim-path* (car p) (cadr p) (cddr p) t))

(define (score-path p)
  (for/sum ([x p])
    (match-define (list a b _) x)
    (* a b)))

(define to-visit-vertices
  (remq 'AA (get-vertices cave-graph)))

(define all-paths
  (sequence-map (lambda~>> (cons 'AA))
                (in-permutations to-visit-vertices)))

(time
  (for/fold ([best 0] [seq #f]) ([p all-paths] [n (in-naturals)])
    (when (zero? (modulo n 10000))
      (displayln (~a (~a #:min-width 9 n) " :: " best " :: " seq)))
    (define p* (trim-path p 30))
    (define s (score-path p*))
    (if (> s best)
        (values s (map caddr p*))
        (values best seq))))

(module* part1 #f)

(module* part2 #f)

