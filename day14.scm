(define-module (day14)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-43))

(define (vector+ . vs)
  "Element-wise sum of vectors."
  (apply vector-map (lambda (i . xs) (apply + xs)) vs))

(define (vector* factor vec)
  "Multiply a vector by a scalar."
  (vector-map (lambda (i x) (* factor x)) vec))

(define (vector-modulo vec modvec)
  "Element-wise modulo of one vector by another, for modular arithmetic."
  (vector-map (lambda (i x mx) (modulo x mx)) vec modvec))

(define (2d-int-vector? obj)
  "Whether the object is a 2-D integer vector."
  (and (vector? obj) (= 2 (vector-length obj)) (vector-every integer? obj)))

(define-record-type
    <robot> (make-robot p v) robot? #| A security robot. |#
    (p robot-position #| The position of the robot, a 2-D integer vector. |#)
    (v robot-velocity #| The velocity of the robot, a 2-D integer vector. |#))

(define +robot-regex+ #| Regular expression for a line of input. |#
  (make-regexp "^p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)$"))

(define (parse-robot line)
  "Parse the given line as a robot specification."
  (define mtch (regexp-exec +robot-regex+ line))
  (unless mtch (error "Expected \"p=<x>,<y> v=<dx><dy>\"." line))
  (match-let (((x y dx dy)
               (map (lambda (n) (string->number (match:substring mtch n)))
                    '(1 2 3 4))))
    (make-robot (vector x y) (vector dx dy))))

(define room-size #| The size of the room. |#
  (make-parameter #(101 103) 
                  (lambda (val) (if (2d-int-vector? val)
                                    val
                                    (error "Must be a 2-D integer vector.")))))

(define time-seconds  #| The time to wait in seconds. |#
  (make-parameter 100
                  (lambda (val) (if (integer? val)
                                    val
                                    (error "Must be an integer number.")))))

(define* (robot-end-position robot)
  "The end position of the robot after time-seconds with the given room-size."
  (vector-modulo (vector+ (vector* (time-seconds) (robot-velocity robot))
                          (robot-position robot))
                 (room-size)))

(define (position-quadrant position middle)
  "The quadrant of the given position vector in the room, or #f.

   Quadrants are numbered from 0 to 3 in some order, and are relative to
   the given middle point."
  (match-let ((#(p1 p2) position) (#(m1 m2) middle))
    (cond ((and (> p1 m1) (> p2 m2)) 0)
          ((and (> p1 m1) (< p2 m2)) 1)
          ((and (< p1 m1) (< p2 m2)) 2)
          ((and (< p1 m1) (> p2 m2)) 3)
          (else #f))))

(define* (robot-stream #:optional (port (current-input-port)))
  "A stream with the robots read from the given input port.

   The stream ends at a blank line or eof.  Closing the port or reading
   from it and attempting to force some previously-unforced value in the
   stream afterwards is an error."
  (stream-let iter ()
    (define line (read-line port))
    (if (or (eof-object? line) (string=? "" line))
        stream-null
        (stream-cons (parse-robot line) (iter)))))

(define* (safety-factor strm)
  "The safety factor of a stream of robots."
  (define acc (make-vector 4 0))
  (define middle (vector* 1/2 (vector+ (room-size) #(-1 -1))))
  (define (add-to-quadrant! quadrant)
    (when quadrant
      (vector-set! acc quadrant (+ 1 (vector-ref acc quadrant)))))
  (define (robot-quadrant robot)
    (position-quadrant (robot-end-position robot) middle))
  (stream-for-each add-to-quadrant! (stream-map robot-quadrant strm))
  (vector-fold (lambda (i st val) (* st val)) 1 acc))

(define* (part1 #:optional (port (current-input-port)))
  (safety-factor (robot-stream port)))

(define (num->char n)
  "Convert from number of robots to an appropriate character for display."
  (if (zero? n) #\space (integer->char (+ (char->integer #\█) n))))

(define (show-positions poss)
  "Display the map of where the positions in the list are.

   The positions must be within the bounds of (room-size)."
  (define mp (apply make-array 0 (vector->list (room-size))))
  (for-each (lambda (p)
              (apply array-set! mp (+ 1 (apply array-ref mp (vector->list p)))
                     (vector->list p)))
            poss)
  (display (string-join (map (lambda (line) (list->string (map num->char line)))
                             (array->list mp))
                        "\n" 'suffix)))

(define (no-duplicates? poss)
  "Return whether there are no duplicates in the list."
  (= (length poss) (length (delete-duplicates poss))))

(define (find-christmas-trees lst)
  "Find and display candidate Christmas trees in the list of robots.

   This must be manually interrupted."
  (define (iter n)
    (define poss (parameterize ((time-seconds n)) (map robot-end-position lst)))
    (when (no-duplicates? poss)
      (display n)
      (newline)
      (show-positions poss))
    (iter (+ n 1)))
  (iter 0))

(define (part2 port)
  (define robots (stream->list (robot-stream port)))
  (find-christmas-trees robots))

