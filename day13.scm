(define-module (day13)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9))

(define-record-type
    <move> (make-move x y) move? #| A move of the claw. |#
    (x move-x #| The move in the X axis, an integer. |#)
    (y move-y #| The move in the Y axis, an integer. |#))

(define-record-type
    <machine> (make-machine a b prize) machine? #| A claw machine. |#
    (a machine-a #| The <move> from pressing claw A for 3 tokens. |#)
    (b machine-b #| The <move> from pressing claw B for 1 token. |#)
    (prize machine-prize #| The position of the prize, a <move>. |#))

(define +button-a-regex+ (make-regexp "^Button A: X\\+([0-9]+), Y\\+([0-9]+)$"))
(define +button-b-regex+ (make-regexp "^Button B: X\\+([0-9]+), Y\\+([0-9]+)$"))
(define +prize-regex+ (make-regexp "^Prize: X=([0-9]+), Y=([0-9]+)$"))

(define (match->move mtch)
  "Convert a match from one of the three regexes into a <move>."
  (make-move (string->number (match:substring mtch 1))
             (string->number (match:substring mtch 2))))

(define (parse-move rx line errmsg)
  "Parse a line into a move by using the given regex.

   The error message is used if the line doesn't match the regex.  Another
   error is raised if the line is an eof-object."
  (when (eof-object? line) (error "Unexpected end of input."))
  (match->move (or (regexp-exec rx line) (error errmsg line))))

(define* (read-machine #:optional (port (current-input-port)))
  "Read a machine from standard input, returning #f on end of file.

   The machine must end with a blank line, which is consumed, or eof."
  (define line1 (read-line port))
  (if (eof-object? line1)
      #f
      (let ((button-a (parse-move +button-a-regex+ line1
                                  "Expected: Button A: X+<num>, Y+<num>."))
            (button-b (parse-move +button-b-regex+ (read-line port)
                                  "Expected: Button B: X+<num>, Y+<num>."))
            (prize (parse-move +prize-regex+ (read-line port)
                               "Expected: Prize: X=<num>, Y=<num>."))
            (blank (read-line port)))
        (unless (or (eof-object? blank) (string=? blank ""))
          (error "A machine must end with an empty line." blank))
        (make-machine button-a button-b prize))))

(define (det2 a1 b1 a2 b2)
  "Get the determinant of the matrix {{a1, b1}, {a2, b2}}."
  (- (* a1 b2) (* b1 a2)))


(define (cramer machine)
  "Get the number of times to push A and B to get a prize by Cramer's rule.

   If the two equations are linearly independent, the two numbers are
   returned as multiple values, but they may be out of range and need not
   be integers.  If they are linearly dependent, #f is returned twice."
  (match-let ((($ <machine> ($ <move> ax ay) ($ <move> bx by) ($ <move> px py))
               machine))
    (let ((det (det2 ax ay bx by)))
      (if (zero? det)
          (values #f #f)
          (values (/ (det2 px py bx by) det)
                  (/ (det2 ax ay px py) det))))))

(define (allowed-push? num)
  "Whether we are allowed to push the given number of times."
  (and (integer? num) (not (negative? num))))

(define (min-tokens machine)
  "The minimum number of tokens to win a prize in the machine, or #f."
  (define-values (a b) (cramer machine))
  (cond ((not a) (error "Linearly dependent case not implemented."))
        ((and (allowed-push? a) (allowed-push? b)) (+ (* 3 a) b))
        (else #f)))

(define* (part1 #:optional (port (current-input-port)))
  (define (iter acc)
    (define machine (read-machine))
    (if machine
        (iter (+ acc (or (min-tokens machine) 0)))
        acc))
  (iter 0))

(define +adjustment+ 10000000000000)

(define (adjust-machine machine)
  "Adjust the machine to add 10000000000000 to each side in the prize."
  (match-let ((($ <machine> a b ($ <move> px py)) machine))
    (make-machine a b (make-move (+ +adjustment+ px) (+ +adjustment+ py)))))

(define* (part2 #:optional (port (current-input-port)))
  (define (iter acc)
    (define machine (read-machine))
    (if machine
        (iter (+ acc (or (min-tokens (adjust-machine machine)) 0)))
        acc))
  (iter 0))
