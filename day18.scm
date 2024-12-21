(define-module (day18)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11))

(define +line-regex+ (make-regexp "^([0-9]+),([0-9]+)$"))

(define (read-coord port)
  (define line (read-line port))
  (if (eof-object? line)
      (values #f #f)
      (let ((mtch (regexp-exec +line-regex+ line)))
        (if mtch
            (values (string->number (match:substring mtch 1))
                    (string->number (match:substring mtch 2)))
            (error "Expecting pair of integer coordinates 'a,b'." line)))))

(define* (make-map
          #:key (port (current-input-port)) (maze-size 71) (steps 1024))
  (define mp (make-typed-array 'b #f maze-size maze-size))
  (define (iter left)
    (unless (zero? left)
      (let-values (((i j) (read-coord port)))
        (unless i (error "Insufficient input."))
        (array-set! mp #t i j))
      (iter (- left 1))))
  (iter steps)
  mp)

(define-record-type <node> (make-node i j dist) node?
                    (i node-i)
                    (j node-j)
                    (dist node-dist))

(define (solve mp)
  (define size (car (array-dimensions mp)))
  (define visited (make-typed-array 'b #f size size))
  (array-copy! mp visited)
  (define q (make-q))
  (enq! q (make-node (- size 1) (- size 1) 0))
  (define (maybe-add! i j dist)
    (when (and (array-in-bounds? mp i j) (not (array-ref visited i j)))
      (enq! q (make-node i j dist))
      (array-set! visited #t i j)))
  (define (add-next! node)
    (match-let ((($ <node> i j dist) node))
      (for-each (lambda (di dj) (maybe-add! (+ i di) (+ j dj) (+ dist 1)))
                '(1 -1 0 0) '(0 0 1 -1))))
  (let bfs ()
    (if (q-empty? q)
        #f                              ; Not connected
        (match-let (((and node ($ <node> i j dist)) (deq! q)))
          (if (= 0 i j)
              dist
              (begin (add-next! node) (bfs)))))))

(define (part1 port)
  (solve (make-map #:port port)))

(define* (find-cutter
          #:key (port (current-input-port)) (maze-size 71))
  (define mp (make-typed-array 'b #f maze-size maze-size))
  (let iter ()
    (let-values (((i j) (read-coord port)))
      (if i
          (begin (array-set! mp #t i j)
                 (if (solve mp) (iter) (cons i j)))
          #f))))

(define (part2 port)
  (find-cutter #:port port))
