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

(define (write-dot-file filename graph vertex-label)
  (call-with-output-file filename
    #:exists 'replace
    (λ (outp)
      (graphviz graph
                #:output outp
                #:vertex-attributes
                (list (list 'label vertex-label))))))

(define (vertex-label v)
  (~a v "\n" (hash-ref flows v)))

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

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 16 #:cache #t)
       #;test-input
       (in-port read-node)
       sequence->stream))

(define-values (flows cave-graph)
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

#;(write-dot-file "16t0.dot" cave-graph vertex-label)
(remove-zero-flow-vertices! cave-graph 'AA flows)
#;(write-dot-file "16t1.dot" cave-graph vertex-label)

(define distance
  (let ([dt (floyd-warshall cave-graph)])
    (λ (u v)
      (define d (hash-ref dt (list u v) #f))
      (and d (inexact->exact d)))))

(define to-visit
  (set-remove (list->set (get-vertices cave-graph)) 'AA))


(module* part1 #f
  (define (find-max cur rest ttl)
    (define (cur-flow)
      (* (sub1 ttl) (hash-ref flows cur)))
    (cond
      [(< ttl 1) 0]
      [(set-empty? rest) (cur-flow)]
      [else
       (for/fold ([m 0]) ([v (in-set rest)])
         (define v-flow
           (find-max v (set-remove rest v) (- ttl (distance cur v) 1)))
         (define t-flow (+ (cur-flow) v-flow))
         (max m t-flow))]))

  (time
    (find-max 'AA to-visit 31)))

(module* part2 #f
  (define (find-max cur0 d0 cur1 d1 rest ttl)
    (define (rest-flow n) (* ttl (hash-ref flows n)))
    (define (prune-reachable)
      (for/set ([vertex (in-set rest)]
                #:when
                (or (< (distance cur0 vertex) ttl)
                    (< (distance cur1 vertex) ttl)))
        vertex))
    (cond
      [(and (> d0 ttl) (> d1 ttl)) 0]
      [(zero? d0)
       (cond
         [(set-empty? rest)
          (+ (find-max cur1 d1 #f +inf.0 rest ttl)
             (rest-flow cur0))]
         [(zero? ttl) 0]
         [else
          (define rest (prune-reachable))
          (for/fold ([m 0]) ([v (in-set rest)])
            (define v-flow
              (find-max cur1 d1
                        v (add1 (distance cur0 v))
                        (set-remove rest v) ttl))
            (define t-flow (+ (rest-flow cur0) v-flow))
            (max m t-flow))])]
      [else
       (define-values (d ncur0 nd0 ncur1 nd1)
         (if (< d0 d1)
             (values d0 cur0 0 cur1 (- d1 d0))
             (values d1 cur1 0 cur0 (- d0 d1))))
       (find-max ncur0 nd0 ncur1 nd1 rest (- ttl d))]))

  (time
    (find-max 'AA 0 'AA 0 to-visit 26)))
