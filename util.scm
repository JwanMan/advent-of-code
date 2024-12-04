(define-module (util)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:export (point point? point-i point-j square array-point point-add read-map))

(define-record-type <point> (point i j) point?
                    (i point-i)
                    (j point-j))

(define (square i1 j1 i2 j2)
  (define (row i)
    (stream-map (cut point i <>) (stream-range j1 j2)))
  (stream-concat (stream-map row (stream-range i1 i2))))

(define (array-point arr pt)
  (array-ref arr (point-i pt) (point-j pt)))

(define (point-add pt i j)
  (point (+ (point-i pt) i) (+ (point-j pt) j)))

(define* (read-map #:optional (port (current-input-port)))
  (define (read-lines acc)
    (define line (read-line port))
    (if (and (not (eof-object? line)) (> (string-length line) 0))
        (read-lines (cons line acc))
        acc))
  (define strings (reverse! (read-lines '())))
  (define string->char-codes (compose (cut map char->integer <>) string->list))
  (list->typed-array 'u8 2 (map string->char-codes strings)))
