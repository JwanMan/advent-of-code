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

(define (backtrack-fast equation concat?)
  (define (attempt? lst result)
    (if (null? (cdr lst))
        (= (car lst) result)
        (or (let ((x (/ result (car lst))))
              (and (integer? x) (attempt? (cdr lst) x)))
            (and concat?
                 (let ((res (number->string result))
                       (x (number->string (car lst))))
                   (and (string-suffix? x res)
                        (> (string-length res) (string-length x))
                        (attempt? (cdr lst)
                                  (string->number
                                   (substring res 0
                                              (- (string-length res)
                                                 (string-length x))))))))
            (let ((diff (- result (car lst))))
              (and (not (negative? diff)) (attempt? (cdr lst) diff))))))
  (attempt? (reverse (equation-operands equation)) (equation-result equation)))

(define (ex-iter port acc nlines concat?)
  (define line (read-line port))
  (when (zero? (modulo nlines 100))
    (format #t "Parsing line ~a...\n" nlines))
  (if (or (eof-object? line) (string= "" line))
      acc
      (let ((eq (parse-equation line)))
        (ex-iter port (if (backtrack-fast eq concat?)
                          (+ acc (equation-result eq))
                          acc)
                 (+ nlines 1)
                 concat?))))

(define* (part1 #:optional (port (current-input-port)))
  (ex-iter port 0 1 #f))

(define* (part2 #:optional (port (current-input-port)))
  (ex-iter port 0 1 #t))
