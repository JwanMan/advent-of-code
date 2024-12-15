(define-module (day12)
	       #:use-module (ice-9 rdelim)
	       #:use-module (srfi srfi-1))

(define (read-input port)
  (define (iter acc)
    (define line (read-line port))
    (if (or (eof-object? line) (string= line ""))
      acc
      (iter (cons (string->list line) acc))))
  (list->array 2 (reverse (iter '()))))

(define (rectangle-fold proc init x0 xpast y0 ypast)
  (let yloop ((y y0) (result init))
    (if (>= y ypast)
        result
        (let xloop ((x x0) (result result))
          (if (>= x xpast)
              (yloop (+ y 1) result)
              (xloop (+ x 1) (proc x y result)))))))

(define (add-margin input)
  (define-values (h w) (apply values (array-dimensions input)))
  (define result
    (make-array #\space (list -1 h) (list -1 w)))
  (rectangle-fold
    (lambda (i j acc)
      (array-set! result (array-ref input i j) i j))
    #f 0 h 0 w)
  result)

(define (get-area mp visited i j)
  (define c (array-ref mp i j))
  (array-set! visited #t i j)
  (fold (lambda (di dj acc)
	  (define ni (+ i di))
	  (define nj (+ j dj))
	  (if (or (array-ref visited ni nj)
		  (not (char=? c (array-ref mp ni nj))))
	    acc
	    (+ acc (get-area mp visited ni nj))))
	1 '(1 -1 0 0) '(0 0 1 -1)))

(define (set-area! mp areas val i j)
  (define c (array-ref mp i j))
  (when (zero? val) (error "Zero val!"))
  (array-set! areas val i j)
  (fold (lambda (di dj acc)
	  (define ni (+ i di))
	  (define nj (+ j dj))
	  (when (and (zero? (array-ref areas ni nj))
		     (char=? c (array-ref mp ni nj)))
	    (set-area! mp areas val ni nj)))
	#f '(1 -1 0 0) '(0 0 1 -1)))

(define (get-areas mp h w)
  (define result (make-array 0 (list -1 h) (list -1 w)))
  (define visited (make-array #f (list -1 h) (list -1 w)))
  (rectangle-fold (lambda (i j acc)
		    (unless (array-ref visited i j)
		      (set-area! mp result
				 (get-area mp visited i j)
				 i j)))
		  #f 0 h 0 w)
  result)

(define (get-costs mp areas h w)
  (rectangle-fold
    (lambda (i j acc)
      (define c (array-ref mp i j))
      (+ acc
	 (if (char=? c (array-ref mp (+ i 1) j))
	   0
	   (+ (array-ref areas i j) (array-ref areas (+ i 1) j)))
	 (if (char=? c (array-ref mp i (+ j 1)))
	   0
	   (+ (array-ref areas i j) (array-ref areas i (+ j 1))))))
    0 -1 h -1 w))

(define (part1 port)
  (define input (read-input port))
  (define-values (h w) (apply values (array-dimensions input)))
  (define mp (add-margin input))
  (define areas (get-areas mp h w))
  (get-costs mp areas h w))

(define (get-discount-costs mp areas h w)
  (define (count di dj)
    (define visited
      (make-array #f (list -1 h) (list -1 w)))
    (define (visit! i j c)
      (unless (or (not (char=? c (array-ref mp i j)))
		  (char=? c (array-ref mp (+ i di) (+ j dj))))
	(array-set! visited #t i j)
	(visit! (if (zero? di) (+ i 1) i)
		(if (zero? dj) (+ j 1) j) c)))
    (lambda (i j acc)
      (if (array-ref visited i j)
	acc
	(let ((c (array-ref mp i j)))
	  (if (char=? c (array-ref mp (+ i di) (+ j dj)))
	    acc
	    (begin (visit! i j c)
		   (+ acc (array-ref areas i j))))))))
  (+ (rectangle-fold (count -1 0) 0 0 h 0 w)
     (rectangle-fold (count 1 0) 0 0 h 0 w)
     (rectangle-fold (count 0 -1) 0 0 h 0 w)
     (rectangle-fold (count 0 1) 0 0 h 0 w)))

(define (part2 port)
  (define input (read-input port))
  (define-values (h w) (apply values (array-dimensions input)))
  (define mp (add-margin input))
  (define areas (get-areas mp h w))
  (get-discount-costs mp areas h w))
