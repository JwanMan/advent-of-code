(define-module (day19)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-158))

(define +allowed-chars+ (char-set #\w #\u #\b #\r #\g))

(define (string->rope-index s)
  (let split ((start 0) (acc '()))
    (define end (string-contains s ", " start))
    (define next-s (substring s start (or end (string-length s))))
    (define next-acc (cons next-s acc))
    (unless (string-every +allowed-chars+ next-s)
      (error "Unrecognized rope color in spec (expected: w,u,b,r,g)." next-s))
    (if end
        (split (+ end 2) next-acc)
        (list->vector (sort! next-acc string<?)))))

(define* (prefix-index ropes s #:optional
                       (sstart 0) (send (string-length s))
                       (rstart 0) (rend (vector-length ropes)))
  (cond ((>= rstart rend) #f)
        ((= rend (+ rstart 1)) rstart)
        (else (let* ((rmid (quotient (+ rstart rend) 2))
                     (rs (vector-ref ropes rmid)))
                (if (string<= rs s 0 (string-length rs) sstart send)
                    (prefix-index ropes s sstart send rmid rend)
                    (prefix-index ropes s sstart send rstart rmid))))))

(define (generate-prefixes ropes s sstart)
  (make-coroutine-generator
   (lambda (yield)
     (let loop ((index (vector-length ropes)) (send (string-length s)))
       (define next (prefix-index ropes s sstart send 0 index))
       (when next
         (let* ((ns (vector-ref ropes next))
                (part (string-prefix-length s ns sstart send)))
           (unless (zero? part)
             (when (= part (string-length ns)) (yield ns))
             (loop next (+ sstart part)))))))))

(define (rope-count-matches ropes line)
  (define dp (make-vector (+ (string-length line) 1) 1))
  (let loop! ((i (- (string-length line) 1)))
    (define (matches-with-prefix p)
      (vector-ref dp (+ i (string-length p))))
    (unless (negative? i)
      (vector-set! dp i
                   (generator-fold + 0
                                   (gmap matches-with-prefix
                                         (generate-prefixes ropes line i))))
      (loop! (- i 1))))
  (vector-ref dp 0))

(define (generate-towel-counts port)
  (define ropestring (read-line port))
  (when (eof-object? ropestring) (error "File is empty!"))
  (define ropes (string->rope-index ropestring))
  (define blank (read-line port))
  (unless (equal? "" blank) (error "Expected a blank line." blank))
  (define (count-matches line)
    (unless (string-every +allowed-chars+ line)
      (error "Unrecognized rope color in line (expected: w,u,b,r,g)." line))
    (rope-count-matches ropes line))
  (gmap count-matches
        (gtake-while (lambda (s) (not (string=? "" s)))
                     (lambda () (read-line port)))))

(define* (part1 #:optional (port (current-input-port)))
  (generator-count positive? (generate-towel-counts port)))

(define* (part2 #:optional (port (current-input-port)))
  (generator-fold + 0 (generate-towel-counts port)))
