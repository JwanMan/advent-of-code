(define-module (day24)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-69))

(define +bit-regex+ (make-regexp "^([a-z0-9]+): ([01])$"))
(define +gate-regex+
  (make-regexp "^([a-z0-9]+) (AND|OR|XOR) ([a-z0-9]+) -> ([a-z0-9]+)$"))

(define (match-values mtch)
  (define (iter i acc)
    (if (zero? i)
        acc
        (iter (- i 1) (cons (match:substring mtch i) acc))))
  (apply values (iter (- (match:count mtch) 1) '())))

(define (do-lines proc port)
  (define line (read-line port))
  (unless (or (eof-object? line) (string=? "" line))
    (proc line)
    (do-lines proc port)))

(define-record-type <gate> (make-gate type in1 in2) gate?
                    (type gate-type)
                    (in1 gate-in1)
                    (in2 gate-in2))

(set-record-type-printer!
 <gate> (lambda (obj port)
          (format port "#<gate ~a ~a ~a>"
                  (gate-in1 obj) (gate-type obj) (gate-in2 obj))))

(define* (read-circuit #:optional (port (current-input-port)))
  (define result (make-hash-table))
  (do-lines
   (lambda (line)
     (define mtch (regexp-exec +bit-regex+ line))
     (unless mtch (error "Expected 'cable: 0|1'." line))
     (define-values (name bit) (match-values mtch))
     (when (hash-table-exists? result name)
       (error "Cable is given a value twice." name))
     (hash-table-set! result name (string=? "1" bit)))
   port)
  (do-lines
   (lambda (line)
     (define mtch (regexp-exec +gate-regex+ line))
     (unless mtch (error "Expected 'cable AND|OR|XOR cable -> cable"))
     (define-values (in1 gate in2 out) (match-values mtch))
     (when (hash-table-exists? result '())
       (error "Cable is given a value twice." out))
     (hash-table-set! result out (make-gate (string->symbol gate) in1 in2)))
   port)
  result)

(define (circuit-eval circuit cable)
  (define val (hash-table-ref circuit cable))
  (if (boolean? val)
      val
      (let ((g1 (gate-in1 val)) (g2 (gate-in2 val)))
        (case (gate-type val)
          ((AND) (and (circuit-eval circuit g1) (circuit-eval circuit g2)))
          ((OR) (or (circuit-eval circuit g1) (circuit-eval circuit g2)))
          ((XOR) (not (eq? (circuit-eval circuit g1)
                           (circuit-eval circuit g2))))))))

(define (circuit-number circuit)
  (let iter ((num 0) (b 0))
    (define cable (format #f "z~2,'0d" b))
    (if (hash-table-exists? circuit cable)
        (iter (if (circuit-eval circuit cable) (+ num (expt 2 b)) num)
              (+ b 1))
        num)))

(define* (part1 #:optional (port (current-input-port)))
  (circuit-number (read-circuit port)))

;;; Best viewed with `dot`
(define* (write-circuit-graph circuit #:optional (port (current-output-port)))
  (define (write-node name val)
    (if (boolean? val)
        (format port "\t~a[shape=point,xlabel=~a]\n" name name)
        (format port "\t~a[shape=rectangle,label=\"~a\",xlabel=~a]\n"
                name
                (case (gate-type val) ((AND) "&") ((OR) ">=1") ((XOR) "=1"))
                name)))
  (define (write-arrows name val)
    (when (gate? val)
      (format port "\t~a -> ~a\n\t~a -> ~a\n"
              (gate-in1 val) name (gate-in2 val) name)))
  (display "digraph G {\nrankdir=LR\n" port)
  (hash-table-walk circuit write-node)
  (newline port)
  (hash-table-walk circuit write-arrows)
  (display "}\n" port))

(define* (swap! circuit n1 n2)
  (define v1 (hash-table-ref circuit n1))
  (define v2 (hash-table-ref circuit n2))
  (hash-table-set! circuit n1 v2)
  (hash-table-set! circuit n2 v1))

(define* (set-inputs! circuit in1 in2)
  (let iter ((in1 in1) (in2 in2) (i 0))
    (define w1 (format #f "x~2,'0d" i))
    (define w2 (format #f "y~2,'0d" i))
    (unless (eq? (hash-table-exists? circuit w1)
                 (hash-table-exists? circuit w2))
      (error "One of the wires exists and the other doesn't." w1 w2))
    (when (hash-table-exists? circuit w1)
      (hash-table-set! circuit w1 (odd? in1))
      (hash-table-set! circuit w2 (odd? in2))
      (iter (quotient in1 2) (quotient in2 2) (+ i 1)))))

(define (test-input circuit)
  (define rand1 (random (expt 2 45)))
  (define rand2 (random (expt 2 45)))
  (set-inputs! circuit rand1 rand2)
  (define result (circuit-number circuit))
  (define expected (+ rand1 rand2))
  (define ok? (= result expected))
  (unless ok?
    (format (current-error-port)
            "Input 1:  ~46b\nInput 2:  ~46b\nExpected: ~46b\nActual:   ~46b\n"
            rand1 rand2 expected result)
    (let* ((diff (- result expected))
           (first-bit (- (integer-length (logand diff (- diff))) 1)))
      (format (current-error-port) "First difference in bit ~a.\n" first-bit)))
  ok?)

(define (random-test circuit trials)
  (or (zero? trials)
      (and (test-input circuit) (random-test circuit (- trials 1)))))

;;; Workflow is:
;;; 1. Use write-circuit-graph to generate a graph, then `dot` to display it.
;;; 2. Run part 2 to find the first bit that fails.
;;; 3. Inspect that part of the graph and add the two swapped gates to +swaps+.
;;; 4. Go back to step 2.
;;; After we collected the 4 pairs, part 2 should work ok!
(define +swaps+ '("z16" "hmk" "z20" "fhp" "rvf" "tpc" "fcd" "z33"))

(define (part2 port)
  ;; Only works with my input
  ;; z16 <--> hmk
  ;; z20 <--> fhp
  ;; tpc <--> rvf
  ;; z33 <--> fcd
  (define circuit (read-circuit port))
  (let swap ((poss +swaps+))
    (unless (null? poss)
      (swap! circuit (car poss) (cadr poss))
      (swap (cddr poss))))
  (random-test circuit 1000)
  (string-join (sort +swaps+ string<?) ","))


