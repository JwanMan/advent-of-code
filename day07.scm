(define-module (day07)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9))

(define +eq-regex+ (make-regexp "^([0-9]+): ([0-9]+( [0-9]+)*)$"))

(define-record-type
    <equation> (make-equation result operands) equation?
    (result equation-result)
    (operands equation-operands))

(define* (parse-equation line)
  (define mtch (regexp-exec +eq-regex+ line))
  (unless mtch
    (error "Expected equation like \"<result>: <op1> [op2] ...\"." line))
  (make-equation (string->number (match:substring mtch 1))
                 (map string->number
                      (string-split (match:substring mtch 2) #\space))))

(define (equation-plausible? equation . ops)
  (define result (equation-result equation))
  (define (attempt? lst)
    (cond ((null? (cdr lst)) (= (car lst) result))
          ((> (car lst) result) #f)
          (else (match-let (((fst snd . rest) lst))
                  (any (lambda (op) (attempt? (cons (op fst snd) rest)))
                       ops)))))
  (attempt? (equation-operands equation)))

(define (ex-iter port acc nlines pred)
  (define line (read-line port))
  (when (zero? (modulo nlines 100))
    (format #t "Parsing line ~a...\n" nlines))
  (if (or (eof-object? line) (string= "" line))
      acc
      (let ((eq (parse-equation line)))
        (ex-iter port (if (pred eq)
                          (+ acc (equation-result eq))
                          acc)
                 (+ nlines 1)
                 pred))))

(define* (part1 #:optional (port (current-input-port)))
  (ex-iter port 0 1 (lambda (eq) (equation-plausible? eq + *))))

(define (|| n1 n2)
  (string->number (string-append (number->string n1) (number->string n2))))

(define* (part2 #:optional (port (current-input-port)))
  (ex-iter port 0 1 (lambda (eq) (equation-plausible? eq + * ||))))
