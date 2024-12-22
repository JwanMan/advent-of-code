(define-module (day21)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-69)
  #:use-module (srfi srfi-89)
  #:use-module (srfi srfi-145))

(define (call-times n proc)
  (do ((i 0 (+ i 1)))
      ((= i n))
    (proc i)))

(define (call-rectangle m n proc)
  (call-times m (lambda (i) (call-times n (lambda (j) (proc i j))))))

(define (matrix? obj)
  (and (array? obj)
       (= 2 (array-rank obj))
       (every integer? (array-dimensions obj))))

(define (keypad-spec? obj)
  (and (matrix? obj)
       (let-values (((m n) (apply values (array-dimensions obj))))
         (and (equal? (sort! (concatenate (array->list obj))
                             (lambda (a b) (and b (or (not a) (< a b)))))
                      (let loop ((acc '()) (i (- (* m n) 2)))
                        (if (negative? i)
                            (cons #f acc)
                            (loop (cons i acc) (- i 1)))))
              (not (every integer?
                          (list (array-ref obj 0 0)
                                (array-ref obj 0 (- n 1))
                                (array-ref obj (- m 1) 0)
                                (array-ref obj (- m 1) (- n 1)))))))))


(define (horizontal-key src dst)
  (cond ((= src dst) 0)
        ((> src dst) 3)
        ((< src dst) 1)))

(define (vertical-key src dst)
  (cond ((= src dst) 0)
        ((> src dst) 2)
        ((< src dst) 4)))

(define (next-keypad keypad next-spec)
  "Compute a keypad from the previous one.

   The keypad is a 5x5 matrix where the position (i,j) contains the cost of
   moving from position i to position j and getting ready to press j, with
   A=0, RIGHT=1, UP=2, LEFT=3, and DOWN=4.  The next-spec is an MxN matrix
   containing each integer from 0 up to MN-2 and #f, with #f on a corner. 
   The return value is a (MN-1)x(MN-1) matrix with the same interpretation
   as the keypad parameter but for the next keypad, which is not necessarily
   directional."
  (assume (and (array? keypad) (equal? (array-dimensions keypad) '(5 5)))
          "The keypad should be a 5x5 matrix." keypad)
  (assume (keypad-spec? next-spec)
          "The next-spec should be a keypad specification." next-spec)
  (define-values (m n) (apply values (array-dimensions next-spec)))
  (define k (- (* m n) 1))
  (define next (make-array #f k k))
  (define (new-dist i1 i2 j1 j2)
    (define vert (vertical-key i1 i2))
    (define horiz (horizontal-key j1 j2))
    (define p1 (and (array-ref next-spec i2 j1)
                    (+ (array-ref keypad 0 vert) (array-ref keypad vert horiz)
                       (array-ref keypad horiz 0))))
    (define p2 (and (array-ref next-spec i1 j2)
                    (+ (array-ref keypad 0 horiz) (array-ref keypad horiz vert)
                       (array-ref keypad vert 0))))
    (+ (cond ((not p1) p2)
             ((not p2) p1)
             (else (min p1 p2)))
       (abs (- i1 i2)) (abs (- j1 j2))))
  (call-rectangle
   m n (lambda (i1 j1)
         (define src (array-ref next-spec i1 j1))
         (when src
           (call-rectangle
            m n (lambda (i2 j2)
                  (define dst (array-ref next-spec i2 j2))
                  (when dst
                    (array-set! next (new-dist i1 i2 j1 j2) src dst)))))))
  next)

(define +keypad0+ (make-array 0 5 5))

(define +directional-keypad-spec+ #2((#f 2 0) (3 4 1)))
(define +numerical-keypad-spec+ #2((7 8 9) (4 5 6) (1 2 3) (#f 0 10)))

(define (make-numeric-keypad n)
  (assume (and (integer? n) (not (negative? n)))
          "n must be a nonnegative integer." n)
  (let iter ((left n) (directional +keypad0+))
    (if (zero? left)
        (next-keypad directional +numerical-keypad-spec+)
        (iter (- left 1) (next-keypad directional +directional-keypad-spec+)))))


(define +keypad1+ (next-keypad +keypad0+ +directional-keypad-spec+))
(define +keypad2+ (next-keypad +keypad1+ +directional-keypad-spec+))
(define +keypad3+ (next-keypad +keypad2+ +numerical-keypad-spec+))

(define (char->num c)
  (assume (and (char? c) (or (char<=? #\0 c #\9) (char=? c #\A)))
          "c should be one of the characters in the numerical pad.")
  (if (char=? c #\A)
      10
      (- (char->integer c) (char->integer #\0))))

(define (button-presses keypad input)
  (assume (string? input) "The input must be a string of keypad characters.")
  (let iter ((i 0) (prev 10) (sum 0))
    (if (= i (string-length input))
        ;; Presses to move from one button in the numeric keypad to the next +
        ;; presses to actually press the buttons.
        (+ sum (string-length input))
        (let ((c (char->num (string-ref input i))))
          (iter (+ i 1) c (+ sum (array-ref keypad prev c)))))))

(define (code-points keypad input)
  (assume (string? input) "The input must be a string." input)
  (define len (string-length input))
  (assume (and (not (zero? len)) (char=? #\A (string-ref input (- len 1))))
          "The last button in the code must be A." input)
  (* (button-presses keypad input)
     (string->number (substring input 0 (- len 1)))))

(define* (solve keypads (port (current-input-port)))
  (define pad (make-numeric-keypad keypads))
  (let iter ((sum 0))
    (define line (read-line port))
    (if (or (eof-object? line) (string=? "" line))
        sum
        (iter (+ sum (code-points pad line))))))

(define* (part1 (port (current-input-port)))
  (solve 2 port))

(define* (part2 (port (current-input-port)))
  (solve 25 port))
