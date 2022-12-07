#lang racket

(require (for-syntax syntax/parse)
         advent-of-code
         memoize
         racket/generator
         threading)

(define-match-expander ->n
  (syntax-parser
    [(->n pat) #'(? string? (app string->number (? number? pat)))]))

(define input-stream
  (~>> (open-aoc-input (find-session) 2022 7 #:cache #t)
       (in-port read-line)
       sequence->stream))

(struct directory (name contents) #:transparent)
(struct file (name size) #:transparent)

(define (parse-directory s)
  (match s
    [(or (? stream-empty?) (stream* (regexp #px"^\\$.*$") _))
     (values (hash) s)]
    [(stream* (regexp #px"^dir (.*)$" (list _ name))
              (app parse-directory vs s))
     (values (hash-set vs name (directory name #f)) s)]
    [(stream* (regexp #px"^(\\d+) (.*)$" (list _ (->n size) name))
              (app parse-directory vs s))
     (values (hash-set vs name (file name size)) s)]))

(define (parse-commands s dir-name contents)
  (match s
    [(or (? stream-empty? s) (stream* (regexp #px"^\\$ cd \\.\\.$") s))
     (values (directory dir-name contents) s)]
    [(stream* (regexp #px"^\\$ ls$") (app parse-directory fs s))
     (parse-commands s dir-name fs)]
    [(stream* (regexp #px"^\\$ cd (.*)$" (list _ subdir-name)) s1)
     (define-values (subdir s2) (parse-commands s1 subdir-name (hash)))
     (parse-commands
      s2
      dir-name
      (hash-set contents (directory-name subdir) subdir))]))

(define (parse-input s)
  (define-values (t s1) (parse-commands (stream-rest s) "/" #f))
  t)

(define/memo (directory-size d)
  (for/sum ([v (in-hash-values (directory-contents d))])
    (match v
      [(? directory?) (directory-size v)]
      [(file _ size) size])))

(define (find-directories dir select? yield)
  (for ([f (in-hash-values (directory-contents dir))]
        #:when (directory? f))
    (find-directories f select? yield))
  (when (select? dir)
    (yield dir)))

(define (find-small-directories-total dir)
  (define (select? d)
    (<= (directory-size d) 100000))
  (for/sum ([d (in-generator (find-directories dir select? yield))])
    (directory-size d)))

(define total-capacity 70000000)
(define total-needed-capacity 30000000)

(define (find-directory-to-delete root)
  (define free-capacity (- total-capacity (directory-size root)))
  (define needed-capacity (- total-needed-capacity free-capacity))
  (define (select? d)
    (>= (directory-size d) needed-capacity))
  (define candidates
    (for/list ([candidate (in-generator
                           (find-directories root select? yield))])
      candidate))
  (directory-size (car (sort candidates < #:key directory-size))))

(define t
  #<<T
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
T
  )

(define inp
  (call-with-input-string t
    (λ~>> (in-port read-line) sequence->stream)))

(module* part1 #f
  )

(module* part2 #f
  )
