(library (day23)
  (export part1 part2)
  (import (ice-9 rdelim)
          (ice-9 regex)
          (pfds sets)
          (guile)
          (srfi srfi-69)
          (srfi srfi-88)
          (srfi srfi-89)))

(define +line-regex+ (make-regexp "^([a-z][a-z])-([a-z][a-z])$"))

(define (node? obj)
  (and (string? obj)
       (= (string-length obj) 2)
       (char<=? #\a (string-ref obj 0) #\z)
       (char<=? #\a (string-ref obj 1) #\z)))

(define (node=? . nodes)
  (apply string=? nodes))

(define (network? obj)
  ;; Also, the table maps from node? to set? of node? (comparator string<?) and
  ;; the graph represented this way should be symmetric and antireflexive.
  (hash-table? obj))

(define (network-connects? nw n1 n2)
  (and (hash-table-exists? nw n1)
       (set-member? (hash-table-ref nw n1) n2)))

(define (make-network)
  (make-hash-table))

(define (network-connect! nw n1 n2)
  (define (add n1 n2)
    (define set (hash-table-ref nw n1 (lambda () (make-set string<?))))
    (hash-table-set! nw n1 (set-insert set n2)))
  (unless (string=? n1 n2)
    (add n1 n2)
    (add n2 n1)))

(define (set-for-each proc set)
  (set-fold (lambda (v base) (proc v)) #f set))

(define* (read-network (port (current-input-port)))
  (define nw (make-network))
  (let iter ()
    (define line (read-line port))
    (unless (or (eof-object? line) (string=? "" line))
      (let ((mtch (regexp-exec +line-regex+ line)))
        (unless mtch (error "Expected a connection line, like 'aa-bb'." line))
        (network-connect! nw (match:substring mtch 1) (match:substring mtch 2)))
      (iter)))
  nw)

(define (network-for-each proc nw)
  (hash-table-walk
   nw (lambda (u set)
        (set-for-each (lambda (v) (when (string<? u v) (proc u v))) set))))

(define* (print-network nw (port (current-output-port)))
  (network-for-each (lambda (u v) (format port "~a-~a\n" u v)) nw))

(define (t-prefix? node)
  (char=? #\t (string-ref node 0)))

(define (count-increment n1 n2)
  ;; Each three-element set is counted as many times as there are permutations
  ;; of the elements where the first one starts with t, so here we compensate
  ;; for that.
  (/ 1/2 (+ 1 (if (t-prefix? n1) 1 0) (if (t-prefix? n2) 1 0))))

(define* (count-3-tuples nw)
  (define count 0)
  (define (parse-node! k set)
    (when (t-prefix? k)
      (set-for-each
       (lambda (v1)
         (set-for-each
          (lambda (v2)
            (when (network-connects? nw v1 v2)
              (set! count (+ count (count-increment v1 v2)))))
          set))
       set)))
  (hash-table-walk nw parse-node!)
  count)

(define* (part1 (port (current-input-port)))
  (count-3-tuples (read-network port)))

(define (generate-dotted-graph nw port)
  (display "graph G {\n" port)
  (display "\tnode[shape=point]\n" port)
  (network-for-each (lambda (u v) (format port "\t~a--~a\n" u v)) nw)
  (display "}\n" port))

(define (find-k-clique nw k)
  ;; We start with the empty clique, and all nodes in the graph as candidates
  ;; for adding to the clique.
  ;; Then, for each candidate node:
  ;; - If the candidate connects with every node in the current clique, we
  ;;   search by expanding the current clique with the current candidate and
  ;;   using as candidates the nodes from the current set of candidates that
  ;;   also connect with this newly-added element.  We do this by recursion.
  ;; - If it doesn't connect to every node of the clique, or if the search above
  ;;   fails, we remove the current element from the current set of candidates,
  ;;   as an optimization.
  (define (recur clique intersection)
    (define (fold-elem elem acc)
      ;; acc can be:
      ;; the set of elements that have not been discarded yet, or
      ;; (cons #t found-clique), if a k-clique has been found.
      (if (pair? acc)
          acc
          (let* ((connected (hash-table-ref nw elem))
                 (found-clique (and (subset? clique connected)
                                    (recur (set-insert clique elem)
                                           (set-intersection acc connected)))))
            (if found-clique
                (cons #t found-clique)
                (set-remove acc elem)))))
    (cond ((= (set-size clique) k) clique)
          ((< (+ (set-size clique) (set-size intersection)) k) #f)
          (else (let ((folded (set-fold fold-elem intersection intersection)))
                  (and (pair? folded) (cdr folded))))))
  (recur (make-set string<?)
         (list->set (map car (hash-table->alist nw)) string<?)))

(define (maximum-clique nw)
  (let binary-search ((low 0) (high (hash-table-size nw)))
    (if (= low high)
        #f
        (let* ((mid (quotient (+ low high) 2))
               (clique (find-k-clique nw mid)))
          (if clique
              (or (binary-search (+ mid 1) high) clique)
              (binary-search low mid))))))

(define* (part2 (port (current-input-port)))
  (define nw (read-network port))
  (define clique (maximum-clique nw))
  (string-join (sort! (set->list clique) string<?) ","))
