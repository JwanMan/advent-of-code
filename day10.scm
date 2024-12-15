(define-module (day10)
	       #:use-module (ice-9 rdelim)
	       #:use-module (srfi srfi-1))

(define (read-input port)
  (define (iter acc)
    (define line (read-line port))
    (if (or (eof-object? line) (string= "" line))
      (list->typed-array 'u8 2 (reverse acc))
      (iter (cons (map
		    (lambda (c) (- (char->integer c) (char->integer #\0)))
		    (string->list line))
		  acc))))
  (iter '()))

(define (dp! inp mp num join cellinit)
  (define-values (m n) (apply values (array-dimensions inp)))
  (unless (negative? num)
   (let iter-row ((i 0))
     (unless (= i m)
       (let iter-cell ((j 0))
	 (unless (= j n)
	   (when (= num (array-ref inp i j))
	     (array-set! 
	       mp
	       (fold (lambda (di dj acc)
		       (if (and (array-in-bounds? inp (+ i di) (+ j dj))
				(= (+ 1 num) (array-ref inp (+ i di) (+ j dj))))
			 (join
				  acc (array-ref mp (+ i di) (+ j dj)))
			 acc))
		     cellinit
		     '(1 -1 0 0) '(0 0 1 -1))
	       i j))
	   (iter-cell (+ j 1))))
       (iter-row (+ i 1))))
   (dp! inp mp (- num 1) join cellinit)))

(define (doit port score join cellinit 9init)
  (define inp (read-input port))
  (define-values (m n) (apply values (array-dimensions inp)))
  (define mp (make-array 0 m n))
  (define idx 0)
  (array-map! mp (lambda (s) (if (= s 9) (9init) cellinit)) inp)
  (dp! inp mp 8 join cellinit)
  ;(display mp)
  (let iter-row ((i 0) (acc 0))
    (if (= i m)
      acc
      (iter-row (+ i 1)
		(let iter-cell ((j 0) (acc acc))
		  (if (= j n)
		    acc
		    (iter-cell (+ j 1)
			       (+ acc (if (= (array-ref inp i j) 0)
					(score (array-ref mp i j))
					0)))))))))

(define (part1 port)
  (define idx 0)
  (doit port length (lambda (a b) (lset-union = a b))
	'()
(lambda () (set! idx (+ idx 1)) (list idx))))

(define (part2 port)
  (doit port values + 0 (lambda () 1)))

