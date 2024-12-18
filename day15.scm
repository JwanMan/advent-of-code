(define-module (day15)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26))

(define (rectangle-fold proc init i0 ipast j0 jpast)
  (let iloop ((i i0) (result init))
    (if (>= i ipast)
        result
        (let jloop ((j j0) (result result))
          (if (>= j jpast)
              (iloop (+ i 1) result)
              (jloop (+ j 1) (proc i j result)))))))

(define-record-type
    <problem> (%make-problem warehouse robot directions) problem?
    #| A representation of a particular puzzle input. |#
    (warehouse problem-warehouse        #|
               The 2-D array representing the warehouse.  Characters must be ; ;
               '#' for a wall, 'O' for an object, '.' for a point, '[' for the ; ;
               first part of a big box and ']' for the second part.  The ; ;
               boundary must be made of wall characters, and characters '[' and ; ;
               ']' must always appear in consecutive pairs. |#)
    (robot problem-robot                #|
           The position of the robot as a pair of integers in the bounds of ; ;
           the warehouse. |#)
    (directions problem-directions      #|
                The directions the robot follows, a string of characters like ; ;
                '<', 'v', '>', or '^' for left, down, right, and up, ; ;
                respectively. |#))

(define (ensure-warehouse-tile arr i j)
  "Ensure the warehouse tile at the given position in the array is valid,
   assuming that there is no box in the array boundaries."
  (case (array-ref arr i j)
    ((#\[) (unless (char=? #\] (array-ref arr i (+ j 1)))
             (error "Mismatched [ character in warehouse.")))
    ((#\]) (unless (char=? #\[ (array-ref arr i (- j 1)))
             (error "Mismatched ] character in warehouse.")))
    ((#\# #\. #\O) #f)
    (else "Expected warehouse tiles to be one of #, O, ., [, ]."
          (array-ref arr i j))))

(define (ensure-warehouse obj)
  "Ensure that the given object meets the conditions for being the
   problem-warehouse of a problem."
  (define (ensure-wall i j acc)
    (unless (char=? #\# (array-ref obj i j))
      (error "Expected the warehouse to have solid walls.")))
  (unless (array? obj)
    (error "Expected warehouse to be an array." obj))
  (let ((dims (array-dimensions obj)))
    (unless (= (length dims) 2)
      (error "Expected 2-dimensional warehouse." obj))
    (unless (every integer? dims)
      (error "Expected warehouse to be 0-indexed." obj))
    (let ((h (first dims)) (w (second dims)))
      (unless (or (zero? h) (zero? w))
        (rectangle-fold ensure-wall #f 0 1 0 w)
        (rectangle-fold ensure-wall #f (- h 1) h 0 w)
        (rectangle-fold ensure-wall #f 0 h 0 1)
        (rectangle-fold ensure-wall #f 0 h (- w 1) w))
      (rectangle-fold (lambda (i j acc) (ensure-warehouse-tile obj i j))
                      #f 0 h 0 w))))

(define (make-problem warehouse robot directions)
  "Create a problem instance with the given data."
  (ensure-warehouse warehouse)
  (unless (and (pair? robot) (integer? (car robot)) (integer? (cdr robot)))
    (error "Expected robot to be a pair of integers (i, j)."))
  (unless (array-in-bounds? warehouse (car robot) (cdr robot))
    (error "Expected the robot to be within the bounds of the warehouse."
           robot warehouse))
  (unless (string? directions)
    (error "Expected the directions to be a string." directions))
  (unless (string-every (lambda (c) (member c '(#\^ #\v #\< #\>))) directions)
    (error "Expected the directions to be in [^v<>]." directions))
  (%make-problem warehouse robot directions))

(define (read-problem port)
  "Read the <problem> from the given input port."
  (define robot #f)
  (define (read-lines acc i)
    (define line (read-line port))
    (if (string=? line "")
        (list->array 2 (reverse acc))
        (begin
          (and=> (and (not robot) (string-index line #\@))
                 (lambda (j)
                   (string-set! line j #\.)
                   (set! robot (cons i j))))
          (read-lines (cons (string->list line) acc) (+ i 1)))))
  (define warehouse (read-lines '() 0))
  (define (read-directions acc)
    (define line (read-line port))
    (if (or (eof-object? line) (string=? line ""))
        (string-concatenate (reverse acc))
        (read-directions (cons line acc))))
  (define directions (read-directions '()))
  (make-problem warehouse robot directions))

(define (problem-dimensions problem)
  "The dimensions of the warehouse in the problem as two values."
  (apply values (array-dimensions (problem-warehouse problem))))

(define* (display-problem problem #:optional (port (current-output-port)))
  "Display the problem object to the given output port."
  (define-values (h w) (problem-dimensions problem))
  (define (iter-warehouse i)
    (unless (= i h)
      (let ((line (list->string (array->list
                                 (array-slice (problem-warehouse problem) i)))))
        (when (= i (car (problem-robot problem)))
          (string-set! line (cdr (problem-robot problem)) #\@))
        (write-line line port))
      (iter-warehouse (+ i 1))))
  (iter-warehouse 0)
  (newline port)
  (write-line (problem-directions problem) port))

(define (char->direction c)
  "Translate a (^,v,<,>) character into a (di,dj) pair of coordinates."
  (case c
    ((#\^) '(-1 . 0))
    ((#\<) '(0 . -1))
    ((#\v) '(1 . 0))
    ((#\>) '(0 . 1))
    (else (error "Invalid character."))))

(define (move-horizontal! problem dj)
  "Move the robot left or right, for dj=-1,1, respectively."
  (define mp (problem-warehouse problem))
  (define i (car (problem-robot problem)))
  (define j0 (cdr (problem-robot problem)))
  (define (can-move? j)
    (define nj (+ j dj))
    (case (array-ref mp i nj)
      ((#\.) #t)
      ((#\#) #f)
      (else (can-move? nj))))
  (define (do-move! j prev)
    (define nj (+ j dj))
    (define nxt (array-ref mp i nj))
    (array-set! mp prev i nj)
    (unless (char=? nxt #\.) (do-move! nj nxt)))
  (when (can-move? j0)
    (do-move! j0 #\.)
    (set-cdr! (problem-robot problem) (+ j0 dj))))

(define (move-vertical! problem di)
  "Move the robot up or down, for di=-1,1, respectively."
  (define mp (problem-warehouse problem))
  (define i0 (car (problem-robot problem)))
  (define j0 (cdr (problem-robot problem)))
  (define to-move (make-hash-table))
  (define gave-up? #f)
  (define (scan-pos! i j)
    (define c (array-ref mp i j))
    (case c
      ((#\#) (set! gave-up? #t))
      ((#\O #\[ #\])
       (unless (hash-ref to-move (cons i j))
         (hash-set! to-move (cons i j) #t)
         (when (char=? c #\[) (scan-pos! i (+ j 1)))  ; I know
         (when (char=? c #\]) (scan-pos! i (- j 1)))
         (unless gave-up? (scan-pos! (+ i di) j))))))
  (scan-pos! (+ i0 di) j0)
  (unless gave-up?
    (let ((lst (sort! (hash-map->list (lambda (k v) k) to-move)
                  (if (< di 0)
                      (lambda (a b) (< (car a) (car b)))
                      (lambda (a b) (> (car a) (car b)))))))
      (for-each (lambda (pos)
                  (array-set! mp (array-ref mp (car pos) (cdr pos))
                              (+ (car pos) di) (cdr pos))
                  (array-set! mp #\. (car pos) (cdr pos)))
                lst)
      (set-car! (problem-robot problem) (+ i0 di)))))

(define (move! problem c)
  "Move the robot in the direction given by the character."
  (define diff (char->direction c))
  (if (zero? (car diff))
      (move-horizontal! problem (cdr diff))
      (move-vertical! problem (car diff))))

(define (iterate! problem)
  (string-for-each (cut move! problem <>) (problem-directions problem)))

(define (warehouse-value warehouse)
  (define-values (h w) (apply values (array-dimensions warehouse)))
  (rectangle-fold
   (lambda (i j acc)
     (+ acc (if (member (array-ref warehouse i j) '(#\O #\[))
                (+ (* 100 i) j)
                0)))
   0 0 h 0 w))

(define* (part1 #:optional (port (current-input-port)))
  (define problem (read-problem port))
  (iterate! problem)
  (display-problem problem)
  (warehouse-value (problem-warehouse problem)))

(define (expand-problem problem)
  "Expand the problem first problem to the second problem."
  (define (expand-char c)
    (case c
      ((#\#) '(#\# #\#))
      ((#\O) '(#\[ #\]))
      ((#\.) '(#\. #\.))
      (else (error "Cannot expand character." c))))
  (define (expand-line line)
    (concatenate (map expand-char line)))
  (define expanded-warehouse
    (list->array 2 (map expand-line (array->list (problem-warehouse problem)))))
  (define new-robot (cons (car (problem-robot problem))
                          (* 2 (cdr (problem-robot problem)))))
  (make-problem expanded-warehouse new-robot (problem-directions problem)))

(define* (part2 #:optional (port (current-input-port)))
  (define problem (expand-problem (read-problem port)))
  (iterate! problem)
  (display-problem problem)
  (warehouse-value (problem-warehouse problem)))
