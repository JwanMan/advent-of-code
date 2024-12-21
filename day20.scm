(define-module (day20)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69))

(define-record-type <race> (make-race map start end) race?
                    (map race-map)
                    (start race-start)
                    (end race-end))

(define* (read-race #:optional (input (current-input-port)))
  (define start #f)
  (define end #f)
  (define (check-char c i j)
    (case c
      ((#\S)
       (when start (error "Two start positions in the map."))
       (set! start (cons i j)))
      ((#\E)
       (when end (error "Two end positions in the map."))
       (set! end (cons i j)))
      ((#\. #\#) #f)
      (else (error "Unexpected character in the map." c))))
  (define (iter acc i)
    (define line (read-line input))
    (if (or (eof-object? line) (string=? "" line))
        (list->typed-array 'b 2 (reverse acc))
        (begin (string-for-each-index
                (lambda (j) (check-char (string-ref line j) i j)) line)
               (iter (cons (map (cut char=? #\# <>) (string->list line)) acc)
                     (+ i 1)))))
  (define mp (iter '() 0))
  (unless start (error "Map doesn't have a start character."))
  (unless end (error "Map doesn't have an end character."))
  (make-race mp start end))

(define (pair+ a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(define (race->hash-table race)
  (define ht (make-hash-table))
  (define (possible-next? pos)
    (cond ((hash-table-exists? ht pos) #f)
          ((not (array-in-bounds? (race-map race) (car pos) (cdr pos)))
           (error "The race goes outside the map." pos))
          (else (not (array-ref (race-map race) (car pos) (cdr pos))))))
  (define (iter p pos)
    (hash-table-set! ht pos p)
    (unless (equal? pos (race-end race))
      (let ((next (filter possible-next?
                     (map (cut pair+ pos <>)
                          '((1 . 0) (0 . 1) (-1 . 0) (0 . -1))))))
        (if (= 1 (length next))
            (iter (+ p 1) (car next))
            (error "There must be exactly one way to continue the race."
                   pos next)))))
  (iter 0 (race-start race))
  ht)

(define-record-type <cheat> (make-cheat start end savings) cheat?
                    (start cheat-start)
                    (end cheat-end)
                    (savings cheat-savings))

(define (list-diffs max-distance)
  (let iter-row ((i max-distance) (acc '()))
    (if (< i 0)
        acc
        (let ((max-col (- max-distance i)))
          (let iter-col ((j (- max-col)) (acc acc))
            (if (> j max-col)
                (iter-row (- i 1) acc)
                (iter-col (+ j 1)
                          (cons (cons i j)
                                (if (zero? i)
                                    acc
                                    (cons (cons (- i) j) acc))))))))))

(define *cheat-diffs* (make-parameter (list-diffs 2)))

(define (cheats ht start start-index)
  (define (check-crossing diff)
    (define end (pair+ start diff))
    (and-let* ((end-index (hash-table-ref/default ht end #f))
               (points (- end-index
                          start-index (abs (car diff)) (abs (cdr diff))))
               ((positive? points)))
      (make-cheat start end points)))
  (filter-map check-crossing (*cheat-diffs*)))

(define *saving-threshold* (make-parameter 100))

(define (count-good-cheats ht start start-index)
  (length (filter! (lambda (c) (>= (cheat-savings c) (*saving-threshold*)))
                   (cheats ht start start-index))))

(define (race-good-cheats race)
  (define ht (race->hash-table race))
  (hash-table-fold ht
                   (lambda (k v prev) (+ prev (count-good-cheats ht k v))) 0))

(define* (part1 #:optional (port (current-input-port)))
  (race-good-cheats (read-race port)))

(define* (part2 #:optional (port (current-input-port)))
  (parameterize ((*cheat-diffs* (list-diffs 20)))
    (race-good-cheats (read-race port))))
