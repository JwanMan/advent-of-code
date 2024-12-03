(define-module (day03)
  #:use-module (srfi srfi-41)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (part1 part2))

(define mul-regexp (make-regexp "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|(don't\\(\\))"))

(define (parse-instruction match)
  (cond ((match:start match 3) 'dont)
        ((match:start match 1) (list 'mul
                                     (string->number (match:substring match 1))
                                     (string->number (match:substring match 2))))
        (else 'do)))

(define* (input-instructions #:optional (port (current-input-port)))
  (stream-let loop ((line (read-line port)) (start 0))
              (if (eof-object? line)
                  stream-null
                  (let ((mch (regexp-exec mul-regexp line start)))
                    (if mch
                        (stream-cons (parse-instruction mch)
                                     (loop line (match:end mch)))
                        (loop (read-line port) 0))))))

(define (mul-op? x)
  (and (pair? x) (eq? 'mul (car x))))

(define* (part1 #:optional (port (current-input-port)))
  (stream-fold + 0
               (stream-map (lambda (op) (apply * (cdr op)))
                           (stream-filter mul-op? (input-instructions port)))))

(define* (part2 #:optional (port (current-input-port)))
  (let ((sum 0) (on? #t))
    (stream-for-each
     (match-lambda
       ('do (set! on? #t))
       ('dont (set! on? #f))
       (('mul x y) (when on? (set! sum (+ sum (* x y))))))
     (input-instructions port))
    sum))
