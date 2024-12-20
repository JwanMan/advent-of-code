(define-module (day16)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (pfds psqs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11))

(define (rectangle-fold proc init i0 ipast j0 jpast)
  (let iloop ((i i0) (result init))
    (if (>= i ipast)
        result
        (let jloop ((j j0) (result result))
          (if (>= j jpast)
              (iloop (+ i 1) result)
              (jloop (+ j 1) (proc i j result)))))))

;;; Is this useful? I don't know, I should have used a profiler I guess
(define-record-type <map> (%make-map inner) map?
                    (inner %map))

(define (map-dimensions mp)
  (apply values (array-dimensions (%map mp))))

(set-record-type-printer!
 <map> (lambda (mp port)
         (define-values (h w) (map-dimensions mp))
         (let iter-row ((i 0))
           (unless (= i h)
             (let iter-cell ((j 0))
               (if (= j w)
                   (begin (newline port) (iter-row (+ i 1)))
                   (begin (write-char (map-ref mp i j) port)
                          (iter-cell (+ j 1)))))))))

(define (string-list->map ls)
  (if (null-list? ls)
      (make-typed-array 'u8 0 0 0)
      (let* ((h (length ls))
             (w (string-length (first ls)))
             (mp (make-typed-array 'u8 0 h w)))
        (let iter-row ((ls ls) (i 0))
          (unless (null-list? ls)
            (let ((s (first ls)))
              (unless (= (string-length s) w)
                (error "Not all strings in the list have the same length." w s))
              (let iter-cell ((j 0))
                (unless (= j w)
                  (array-set! mp (char->integer (string-ref s j)) i j)
                  (iter-cell (+ j 1)))))
            (iter-row (cdr ls) (+ i 1))))
        (%make-map mp))))

(define (map-set! mp c i j)
  (array-set! (%map mp) (char->integer c) i j))

(define (map-ref mp i j)
  (integer->char (array-ref (%map mp) i j)))

(define (map-in-bounds? mp i j)
  (array-in-bounds? (%map mp) i j))

(define* (read-map #:optional (port (current-input-port)))
  (define (iter acc)
    (define line (read-line port))
    (if (or (eof-object? line) (string=? "" line))
        (string-list->map (reverse! acc))
        (iter (cons line acc))))
  (iter '()))

(define-record-type <graph> (%make-graph map start end) graph?
                    (map graph-map)
                    (start graph-start)
                    (end graph-end))

(define-record-type <node> (%make-node i j dir) node?
                    (i node-i)
                    (j node-j)
                    (dir node-dir))

(define (%pair->node pair)
  (%make-node (car pair) (cdr pair) 0))

(define* (read-graph #:optional (port (current-input-port)))
  (define mp (read-map port))
  (define-values (h w) (map-dimensions mp))
  (define (find-ends i j acc)
    (case (map-ref mp i j)
      ((#\S)
       (when (car acc) (error "Two start positions." (car acc) (cons i j)))
       (set-car! acc (cons i j))
       (map-set! mp #\. i j))
      ((#\E)
       (when (cdr acc) (error "Two end positions." (cdr acc) (cons i j)))
       (set-cdr! acc (cons i j))
       (map-set! mp #\. i j))
      ((#\. #\#) #f)
      (else (error "Unexpected character in map." (map-ref mp i j))))
    acc)
  (define ends (rectangle-fold find-ends (cons #f #f) 0 h 0 w))
  (unless (car ends) (error "No start position." mp))
  (unless (cdr ends) (error "No end position." mp))
  (%make-graph mp (%pair->node (car ends)) (cdr ends)))

(define (graph-end? grph node)
  (and (= (node-i node) (car (graph-end grph)))
       (= (node-j node) (cdr (graph-end grph)))))

(define (graph-advance grph node)
  (match-let ((($ <node> i j dir) node))
    (let-values (((di dj) (case dir
                            ((0) (values 0 1))
                            ((1) (values -1 0))
                            ((2) (values 0 -1))
                            ((3) (values 1 0)))))
      (let ((ni (+ i di)) (nj (+ j dj)))
        (and (map-in-bounds? (graph-map grph) ni nj)
             (not (char=? #\# (map-ref (graph-map grph) ni nj)))
             (%make-node ni nj dir))))))

(define (node-rotate node diff)
  (%make-node (node-i node) (node-j node) (modulo (+ (node-dir node) diff) 4)))

(define (node< a b)
  (or (< (node-i a) (node-i b))
      (and (= (node-i a) (node-i b))
           (or (< (node-j a) (node-j b))
               (and (= (node-j a) (node-j b)) (< (node-dir a) (node-dir b)))))))

(define (psq-start grph)
  (psq-set (make-psq
            (lambda (a b)
              (or (node< (third a) (third b))
                  (and (equal? (third a) (third b))
                       (node< (car a) (car b)))))
            <)
           (list (graph-start grph) 0 (graph-start grph)) 0))

(define (add-next-nodes psq grph node dist)
  (define (add-node psq new-node dist)
    (psq-set psq (list new-node dist node) dist))
  (define new-psq (add-node (add-node psq (node-rotate node 1) (+ dist 1000))
                            (node-rotate node -1) (+ dist 1000)))
  (define advance (graph-advance grph node))
  (if advance (add-node new-psq advance (+ dist 1)) new-psq))

(define (dijkstra grph)
  (define-values (h w) (map-dimensions (graph-map grph)))
  (define visited (make-typed-array 'b #f h w 4))
  (define (loop psq)
    (if (psq-empty? psq)
        #f                             ; Start is not connected to end
        (let-values (((info npsq) (psq-pop psq)))
          (match-let ((((and node ($ <node> i j dir)) dist prev) info))
            (if (array-ref visited i j dir)
                (loop npsq)
                (begin
                  (array-set! visited #t i j dir)
                  (if (graph-end? grph node)
                      dist
                      (loop (add-next-nodes psq grph node dist)))))))))
  (loop (psq-start grph)))

(define (part1 port)
  (dijkstra (read-graph port)))

(define (dijkstra2 grph)
  (define-values (h w) (map-dimensions (graph-map grph)))
  (define prevtab (make-array '() h w 4))
  (define optimal #f)
  (define (loop psq)
    (if (psq-empty? psq)
        #f
        (let-values (((info npsq) (psq-pop psq)))
          (match-let ((((and node ($ <node> i j dir)) dist prev) info))
            (if (and optimal (> dist optimal))
                optimal
                (match (array-ref prevtab i j dir)
                  (()
                   (array-set! prevtab (list dist prev) i j dir) ; Not visited
                   (when (graph-end? grph node) (set! optimal dist))
                   (loop (add-next-nodes npsq grph node dist)))
                  ((olddist . oldprevs)
                   (when (= olddist dist)
                     (set-cdr! (array-ref prevtab i j dir) (cons prev oldprevs)))
                   (loop npsq))))))))
  (values (loop (psq-start grph)) prevtab))

(define (for-each-dir proc)
  (proc 0) (proc 1) (proc 2) (proc 3))

(define (count-visited grph)
  (define-values (h w) (map-dimensions (graph-map grph)))
  (define-values (dist prevtab) (dijkstra2 grph))
  (define visited (make-typed-array 'b #f h w 4))
  (define (position-visited? i j)
    (any (lambda (dir) (array-ref visited i j dir)) '(0 1 2 3)))
  (define (dfs node)
    (match-let ((($ <node> i j dir) node))
      (unless (array-ref visited i j dir)
        (array-set! visited #t i j dir)
        (let ((prev (array-ref prevtab i j dir)))
          (unless (null-list? prev)
            (for-each dfs (cdr (array-ref prevtab i j dir))))))))
  (define end (graph-end grph))
  (for-each-dir (lambda (dir) (dfs (%make-node (car end) (cdr end) dir))))
  (rectangle-fold
   (lambda (i j cnt) (if (position-visited? i j)  (+ cnt 1) cnt))
   0 0 h 0 w))

(define (part2 port)
  (count-visited (read-graph port)))
