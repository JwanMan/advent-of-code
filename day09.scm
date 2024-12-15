(define-module (day09)
		#:use-module (ice-9 rdelim)
		#:use-module (srfi srfi-9))

(define (check-spot disk-offset file-id times)
  ; n+(n+1)+...+(n+k-1)=k*(2*n+k-1)/2=k*(n+(k-1)/2)
  (* file-id times (+ disk-offset (/ (- times 1) 2))))

(define (num-at s pos)
  (- (char->integer (string-ref s pos))
     (char->integer #\0)))

(define (compact-checksum s)
  (define (iter-used offset sid eid etimes acc)
    (if (= sid eid)
      (+ acc (check-spot offset sid etimes))
      (let ((times (num-at s (* 2 sid))))
	(iter-free (+ offset times)
		   sid
		   (num-at s (+ 1 (* 2 sid)))
		   eid
		   etimes
		   (+ acc (check-spot offset sid times))))))
  (define (iter-free offset sprevid stimes eid etimes acc)
    (cond ((= sprevid eid) acc)
	  ((>= stimes etimes)
	   (iter-free (+ offset etimes)
		      sprevid
		      (- stimes etimes)
		      (- eid 1)
		      (num-at s (* 2 (- eid 1)))
		      (+ acc (check-spot offset eid etimes))))
	  (else (iter-used (+ offset stimes)
			   (+ sprevid 1)
			   eid
			   (- etimes stimes)
			   (+ acc (check-spot offset eid stimes))))))
  (define last-id (quotient (- (string-length s) 1) 2))
  (iter-used 0 0 last-id (num-at s (* 2 last-id)) 0))

(define* (part1 #:optional (input (current-input-port)))
	 (compact-checksum (read-line input)))

(define-record-type <zone> (zone offset length id) zone?
  (offset zone-offset)
  (length zone-length)
  (id zone-id))

(define (displace-free z n)
  (zone (+ (zone-offset z) n)
	(- (zone-length z) n)
	(zone-id z)))

(define (string->free+revused s)
  (define (iter id off free used)
    (if (>= (* 2 id) (string-length s))
      (values (reverse free) used)
      (let ((freelen (num-at s (- (* 2 id) 1)))
	    (usedlen (num-at s (* 2 id))))
        (iter (+ 1 id)
	      (+ off freelen usedlen)
	      (cons (zone off freelen #f) free)
	      (cons (zone (+ off freelen) usedlen id) used)))))
  (iter 1 (num-at s 0) '() (list (zone 0 (num-at s 0) 0))))

(define (defrag-checksum free revused)
  (define (iter-used lst sum)
    (if (null? lst)
      sum
      (let ((off (zone-offset (car lst)))
	    (len (zone-length (car lst))))
	(define (reserve-free! lst)
	  (cond ((null? lst) off)
		((> (zone-offset (car lst)) off) off)
		((>= (zone-length (car lst)) len)
		 (let ((new-off (zone-offset (car lst))))
		   (set-car! lst (displace-free (car lst) len))
		   new-off))
		(else (reserve-free! (cdr lst)))))
	(define new-off (reserve-free! free))
	(iter-used (cdr lst) (+ sum (check-spot new-off (zone-id (car lst)) len))))))
  (iter-used revused 0))

(define* (part2 #:optional (input (current-input-port)))
	 (call-with-values (lambda () (string->free+revused (read-line input)))
			   defrag-checksum))


