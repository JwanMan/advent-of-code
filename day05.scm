(define-module (day05)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69))

(define (make-ordering-rules)
  "Create a new set of ordering rules."
  (make-hash-table))

(define (hash-table-ref! hash key thunk)
  "Like @code{hash-ref} but if the @code{key} is not in the @code{hash},
   it sets it to the value returned by the @code{thunk}, rather than merely
   returning that value."
  (define val (hash-table-ref hash key thunk))
  (unless (hash-table-exists? hash key)
    (hash-table-set! hash key val))
  val)

(define (ordering-rules-add! rules before after)
  "Add the rule that the page numbered @code{before} goes before the page
   numbered @code{after}."
  (define ref (hash-table-ref! rules before make-hash-table))
  (hash-table-set! ref after #t))

(define (ordering-rules-before? rules before after)
  "Check if a page number goes directly before another according to
   @code{rules}.  The relation from @code{before} to @code{after} must
   have been added explicitly."
  (and=> (hash-table-ref/default rules before #f)
         (cut hash-table-ref/default <> after #f)))

(define ordering-regexp (make-regexp "^([0-9]+)\\|([0-9]+)$"))

(define (parse-ordering-rule line)
  "Parse an ordering rule like '47|53' ('<before>|<after>') into two values:
   the page number that goes before and the one that goes after."
  (define mtch (regexp-exec ordering-regexp line))
  (unless mtch
    (error "Expected '<num_before>|<num_after>' ordering specification, got: ~a"
           line))
  (values (string->number (match:substring mtch 1))
          (string->number (match:substring mtch 2))))

(define (read-ordering-rules port)
  "Read a list of ordering rules from the port into a set of ordering rules,
   until a newline or end of file is reached."
  (define rules (make-ordering-rules))
  (let loop ()
    (let ((line (read-line port)))
      (if (or (eof-object? line) (string=? "" line))
          rules
          (call-with-values (lambda () (parse-ordering-rule line))
            (lambda (before after)
              (ordering-rules-add! rules before after)
              (loop)))))))

(define page-update-regexp (make-regexp "^([0-9]+,)*[0-9]+$"))

(define (read-manual-update port)
  "Read an update in a manual as a comma-separated list of page number,
   returning the list of such numbers."
  (define line (read-line port))
  (cond ((eof-object? line) line)
        ((regexp-exec page-update-regexp line)
         (map string->number (string-split line #\,)))
        (#t (error
             "Expected comma-separated list of numbers without spaces, got: ~a"
             line))))

(define (pages-ordered? rules pages)
  "Check if a list of @code{pages} follows a set of ordering @code{rules}."
  (or (null-list? pages)
      (null-list? (cdr pages))
      (and (ordering-rules-before? rules (first pages) (second pages))
           (pages-ordered? rules (cdr pages)))))

(define (middle-page pages)
  "Get the middle page number, assuming the number of pages is odd.
   Otherwise raise an exception."
  (define len (length pages))
  (define mid (if (odd? len)
                  (/ (- len 1) 2)
                  (error "Expected an odd number of pages, got ~a pages." len)))
  (list-ref pages mid))

(define (sort-manual! rules pages)
  "Sort the pages destructively, according to the rules."
  (sort! pages (cut ordering-rules-before? rules <> <>)))

(define (part1 port)
  (define rules (read-ordering-rules port))
  (let loop ((acc 0))
    (define pages (read-manual-update port))
    (cond ((eof-object? pages) acc)
          ((pages-ordered? rules pages) (loop (+ acc (middle-page pages))))
          (#t (loop acc)))))

(define (part2 port)
  (define rules (read-ordering-rules port))
  (let loop ((acc 0))
    (define pages (read-manual-update port))
    (cond ((eof-object? pages) acc)
          ((pages-ordered? rules pages) (loop acc))
          (#t (loop (+ acc (middle-page (sort-manual! rules pages))))))))

