#!/usr/bin/env guile
!#

(use-modules (ice-9 format) (ice-9 regex))

(define args (program-arguments))

(unless (<= 2 (length args) 3)
  (error (format #f "Syntax: ~a <DAY><a|b> [INFILE]\n" (car args)))
  (exit 1))

(define mtch (string-match "^([0-9]+)(a|b)$" (list-ref args 1)))

(unless mtch
  (error (format #f "Expected day and part (like \"03a\" or \"02b\"), got: ~a"
                 (list-ref args 1))))

(define day (format #f "~2,'0d" (string->number (match:substring mtch 1))))
(define part (if (string= (match:substring mtch 2) "a") 'part1 'part2))

(set! %load-path (cons "." %load-path))

(define day-module
  (resolve-module (list (string->symbol (string-append "day" day)))))
(define func (module-ref day-module part))

(define input-file
  (cond ((= (length args) 2) (string-append day ".in"))
        ((string= (list-ref args 2) "-") #f)
        (else (list-ref args 2))))

(display (if input-file
             (call-with-input-file input-file func)
             (func (current-input-port))))
(newline)
