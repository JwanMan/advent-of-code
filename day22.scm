(define-module (day22)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-89)
  #:use-module (srfi srfi-145)
  #:use-module (srfi srfi-158))

(define (evolve n)
  (assume (integer? n) "n must be an integer." n)
  (define n1 (logand #xffffff (logxor n (ash n 6))))
  (define n2 (logxor n1 (ash n1 -5)))
  (logand #xffffff (logxor n2 (ash n2 11))))

(define (call^ n f x)
  (assume (and (integer? n) (not (negative? n)))
          "n must be a nonnegative integer." n)
  (if (zero? n)
      x
      (call^ (- n 1) f (f x))))

(define (answer n)
  (call^ 2000 evolve n))

(define* (part1 (port (current-input-port)))
  (let iter ((sum 0))
    (define line (read-line port))
    (if (or (eof-object? line) (string=? "" line))
        sum
        (iter (+ sum (answer (or (string->number line)
                                 (error "Not a number." line))))))))

(define +dp-dimensions+ '((-9 9) (-9 9) (-9 9) (-9 9)))

(define (dp-table? obj)
  (and (array? obj) (equal? (array-dimensions obj) +dp-dimensions+)))

(define (make-dp-table)
  (apply make-typed-array 'u16 0 +dp-dimensions+))

(define (generate-prices secret)
  (gtake (make-unfold-generator (lambda (n) #f)
                                (lambda (n) (modulo n 10))
                                evolve
                                secret)
         2001))

(define (dp-add-sequence! dp secret)
  (define visited (apply make-typed-array 'b #f +dp-dimensions+))
  (define gen (generate-prices secret))
  (define (iter prev d3 d2 d1)
    (define next (gen))
    (unless (eof-object? next)
      (let ((d0 (- next prev)))
        (when (and d3 (not (array-ref visited d3 d2 d1 d0)))
          (array-set! dp (+ next (array-ref dp d3 d2 d1 d0)) d3 d2 d1 d0)
          (array-set! visited #t d3 d2 d1 d0))
        (iter next d2 d1 d0))))
  (iter (gen) #f #f #f))

(define (dp-max dp)
  (apply max (concatenate (concatenate (concatenate (array->list dp))))))

(define* (part2 (port (current-input-port)))
  (define dp (make-dp-table))
  (let iter ()
    (define line (read-line port))
    (if (or (eof-object? line) (string=? "" line))
        (dp-max dp)
        (begin (dp-add-sequence! dp (string->number line))
               (iter)))))
