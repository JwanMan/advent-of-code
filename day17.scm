(define-module (day17)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))

(define-record-type
    <machine> (%make-machine a b c program) machine? #| A 3-bit computer. |#
    (a machine-a set-machine-a! #| Value of register A, an integer. |#)
    (b machine-b  #| Value of register B, an integer. |#)
    (c machine-c #| Value of register C, an integer. |#)
    (program machine-program
             #| Stored program, a bytevectors with bytes from 0 to 7. |#))

(define (make-machine a b c program)
  "Create a machine with the instruction pointer set to 0.

   The program is a list of integers from 0 to 7."
  (unless (integer? a) (error "Register A should hold an integer." a))
  (unless (integer? b) (error "Register B should hold an integer." b))
  (unless (integer? c) (error "Register C should hold an integer." c))
  (unless (and (list? program)
               (every (lambda (x) (and (integer? x) (<= 0 x 7))) program))
    (error "The program should be a list of integers from 0 to 7." program))
  (%make-machine a b c (list->u8vector program)))

(define instruction-names #("adv" "bxl" "bst" "jnz" "bxc" "out" "bdv" "cdv"))
(define operand-names #("0" "1" "2" "3" "A" "B" "C" "$U7" ""))

(define (display-instruction ins op port)
  (format port "~a ~a\n"
          (vector-ref instruction-names ins)
          (cond ((or (= ins 1) (= ins 3)) op)
                ((= ins 4) "")
                (else (vector-ref operand-names op)))))

(set-record-type-printer!
 <machine>
 (lambda (m port)
   (match-let* ((($ <machine> a b c program) m)
                (len (u8vector-length program)))
     (format port "#<machine Register A: ~a\nRegister B: ~a\nRegister C: ~a\n\n"
             a b c)
     (write-line "Program:" port)
     (do ((i 0 (+ i 2)))
         ((>= (+ i 1) len))
       (display-instruction
        (u8vector-ref program i) (u8vector-ref program (+ i 1)) port))
     (when (odd? len)
       (write-line
        (vector-ref instruction-names (u8vector-ref program (- len 1)))))
     (display ">" port))))

;;; B = A % 8 ^ 1
;;; A >>= 3
;;; out(A ^ B ^ (A >> B))
;;; while A

(define +register-regex+ (make-regexp "^Register (.): (0|[1-9][0-9]*)$"))
(define +program-regex+ (make-regexp "^Program: ([0-7](,[0-7])*)?$"))

(define (read-register name port)
  (define line (read-line port))
  (when (eof-object? line) (error "Unexpected end of file reading register."))
  (define mtch (regexp-exec +register-regex+ line))
  (unless mtch (error "Expected register specification." line))
  (unless (string=? name (match:substring mtch 1))
    (error "Found specification for the wrong register." name line))
  (string->number (match:substring mtch 2)))

(define* (read-machine #:optional (port (current-input-port)))
  "Read the puzzle input as a <machine>."
  (define a (read-register "A" port))
  (define b (read-register "B" port))
  (define c (read-register "C" port))
  (define blank (read-line port))
  (unless (equal? "" blank) (error "Expected blank line." blank))
  (define progline (read-line port))
  (when (eof-object? progline) (error "Expected program, got EOF."))
  (define mtch (regexp-exec +program-regex+ progline))
  (unless mtch (error "Expected valid program." progline))
  (define progdata (match:substring mtch 1))
  (define program (if progdata
                      (map string->number (string-split progdata #\,))
                      '()))
  (make-machine a b c program))

(define* (run machine #:optional (outproc display))
  "Run the machine, without updating the register values back."
  (match-let ((($ <machine> a b c program) machine)
              (ip 0))
    (define size (u8vector-length program))
    (define operands
      (vector (lambda () 0)
              (lambda () 1)
              (lambda () 2)
              (lambda () 3)
              (lambda () a)
              (lambda () b)
              (lambda () c)
              (lambda () (error "Invalid operand 7."))))
    (define (val arg) ((vector-ref operands arg)))
    (define instructions
      (vector (lambda (arg) (set! a (ash a (- (val arg)))))
              (lambda (arg) (set! b (logxor b arg)))
              (lambda (arg) (set! b (modulo (val arg) 8)))
              (lambda (arg) (unless (zero? a) (set! ip arg)))
              (lambda (arg) (set! b (logxor b c)))
              (lambda (arg) (outproc (modulo (val arg) 8)))
              (lambda (arg) (set! b (ash a (- (val arg)))))
              (lambda (arg) (set! c (ash a (- (val arg)))))))
    (define (iter)
      (unless (>= ip size)
        (let ((ins (u8vector-ref program ip))
              (op (u8vector-ref program (+ ip 1))))
          (set! ip (+ ip 2))
          ((vector-ref instructions ins) op)
          (iter))))
    (iter)))

(define* (part1 #:optional (port (current-input-port)))
  (define machine (read-machine port))
  (define out '())
  (run machine (lambda (b) (set! out (cons (number->string b) out))))
  (string-join (reverse out) ","))


(define +goal+ #u8(2 4 1 1 7 5 0 3 1 4 4 4 5 5 3 0))

#|
The program does something like the following:
    REPEAT
        b <- (a AND 7) XOR 1
        OUTPUT((b XOR (a RSHIFT b) XOR 4) AND 7)
        a <- a RSHIFT 3
    UNTIL a == 0

Therefore the value of A in the beginning of an iteration determines
the rest of the input, and because of the RSHIFT, the value of A in the
previous iteration was A*8 + some number between 0 and 7, that is, the
same number with one octal digit added to the right.  Thus we just have
to start with the minimum value of A from 0 to 7 that will print the
last octal digit (octit?) and backtrack adding digits to the right that
result in outputting the corresponding characters to the left of the
program code.  Finally we take this list and parse it as the value of A.
|#
(define (find-a)
  (define (next-output a)
    (define b (logxor (logand a 7) 1))
    (logand (logxor b (ash a (- b)) 4) 7))
  (define (recurse i preva)
    (if (< i 0)
        '()
        (let ((starter (* 8 preva)))
          (let iter ((j 0))
            (if (= j 8)
                #f
                (let* ((x (+ starter j))
                       (out (next-output x)))
                  (or (and (= out (u8vector-ref +goal+ i))
                           (and=> (recurse (- i 1) x)
                                  (lambda (rec) (cons j rec))))
                      (iter (+ j 1)))))))))
  (define octit-list (recurse (- (u8vector-length +goal+) 1) 0))
  (string->number (string-concatenate (map number->string octit-list)) 8))
8
