(define-module (day06)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-69))

(define-record-type <position> (%make-position x y dir) position?
                    (x position-x)
                    (y position-y)
                    (dir position-dir))

(define (make-position x y dir)
  (unless (integer? x) (error "position: x should be an integer." x))
  (unless (integer? y) (error "position: y should be an integer." y))
  (unless (and (integer? dir) (<= 0 dir 3))
    (error "position: dir should be an integer between 0 and 3." dir))
  (%make-position x y dir))

(define* (position-rotate pos #:optional (n 1))
  (match-let ((($ <position> x y dir) pos))
    (make-position x y (modulo (+ n (position-dir pos)) 4))))

(define +num-directions+ 4)
(define +visit-bits+ 15)
(define +obstacle+ 16)
(define +unvisited+ 0)

(define (make-map strlist)
  (define (char->spec c) (if (char=? c #\#) +obstacle+ +unvisited+))
  (list->typed-array 'u8 2
                     (map (lambda (str) (map char->spec (string->list str)))
                          strlist)))

(define (map-obstacle? map x y)
  (logtest +obstacle+ (array-ref map y x)))

(define (map-dimensions map)
  (apply values (array-dimensions map)))

(define (map-visited-dir? map pos)
  (logbit? (position-dir pos)
           (array-ref map (position-y pos) (position-x pos))))

(define (map-visited? map x y)
  (logtest +visit-bits+ (array-ref map y x)))

(define (map-visit! map pos)
  (define prev (array-ref map (position-y pos) (position-x pos)))
  (when (logtest prev +obstacle+)
    (error "map-visit!: Cannot visit a place with an obstacle."))
  (array-set! map (logior prev (ash 1 (position-dir pos)))
              (position-y pos) (position-x pos)))

(define (rectangle-fold proc init x0 xpast y0 ypast)
  (let yloop ((y y0) (result init))
    (if (>= y ypast)
        result
        (let xloop ((x x0) (result result))
          (if (>= x xpast)
              (yloop (+ y 1) result)
              (xloop (+ x 1) (proc x y result)))))))

(define (find-guard strlist)
  (let yloop ((rows strlist) (y 0) (result #f))
    (if (null? rows)
        result
        (let ((row (car rows)))
          (let xloop ((x 0) (result result))
            (define (set-guard dir)
              (define found-guard (make-position x y dir))
              (if result
                  (error "There are at least two guards." result found-guard)
                  (make-position x y dir)))
            (if (>= x (string-length row))
                (yloop (cdr rows) (+ y 1) result)
                (xloop (+ x 1)
                       (case (string-ref row x)
                         ((#\# #\.) result)
                         ((#\^) (set-guard 0))
                         ((#\>) (set-guard 1))
                         ((#\v) (set-guard 2))
                         ((#\<) (set-guard 3))
                         (else (error "Invalid character in the map."
                                      x y (string-ref row x)))))))))))

(define* (read-map #:optional (port (current-input-port)))
  (let loop ((acc '()))
    (let ((line (read-line port)))
      (if (or (eof-object? line) (string= "" line))
          (let ((strlist (reverse! acc)))
            (values (make-map strlist) (find-guard strlist)))
          (loop (cons line acc))))))

(define (position-next pos map)
  (define x (position-x pos))
  (define y (position-y pos))
  (define-values (h w) (apply values (array-dimensions map)))
  (case (position-dir pos)
    ((0) (if (= y 0) #f (make-position x (- y 1) 0)))
    ((1) (if (= (+ x 1) w) #f (make-position (+ x 1) y 1)))
    ((2) (if (= (+ y 1) h) #f (make-position x (+ y 1) 2)))
    ((3) (if (= x 0) #f (make-position (- x 1) y 3)))))

(define (step-guard map guard)
  (define next (position-next guard map))
  (cond ((not next) #f)
        ((map-obstacle? map (position-x next) (position-y next))
         (make-position (position-x guard)
                        (position-y guard)
                        (modulo (+ 1 (position-dir guard)) 4)))
        (else next)))

(define* (part1 #:optional (port (current-input-port)))
  (define-values (mp guard) (read-map port))
  (let loop ((pos guard))
    (when (map-visited-dir? mp pos) (error "Loop detected!"))
    (map-visit! mp pos)
    (and=> (step-guard mp pos) loop))
  (define-values (h w) (map-dimensions mp))
  (rectangle-fold (lambda (x y cnt) (if (map-visited? mp x y) (+ cnt 1) cnt))
               0 0 h 0 w))

(define (map-add-obstacle! mp x y)
  (array-set! mp (logior (array-ref mp y x) +obstacle+) y x))

(define (copy-map mp)
  (define cp (apply make-array 0 (array-dimensions mp)))
  (array-copy! mp cp)
  cp)

(define (do-guard! mp pos)
  (if (map-visited-dir? mp pos)
      'loop
      (let ((next (step-guard mp pos)))
        (map-visit! mp pos)
        (if next
            (do-guard! mp next)
            'exit))))

(define (would-loop? mp initpos x y)
  (define new-map (copy-map mp))
  (map-add-obstacle! new-map x y)
  (eq? (do-guard! new-map initpos) 'loop))

(define* (part2 #:optional (port (current-input-port)))
  (define-values (mp initpos) (read-map port))
  (define-values (h w) (map-dimensions mp))
  (define visits (copy-map mp))
  (do-guard! visits initpos)
  (rectangle-fold
   (lambda (x y cnt)
     (when (= x 0)
       (format #t "Parsing line ~a/~a...\n" (+ y 1) h))
     (if (and (map-visited? visits x y)
              (not (and (= x (position-x initpos)) (= y (position-y initpos))))
              (would-loop? mp initpos x y))
         (+ cnt 1)
         cnt))
   0 0 w 0 h))

;;; EVERYTHING BELOW THIS LINE DOESN'T WORK AND IT'S ME TRYING TO BE
;;; SMART RATHER THAN BRUTE-FORCING THE WHOLE THING
;; (define-record-type <node> (%make-node tree depth parents loopy?) node?
;;                     (tree node-tree)
;;                     (depth node-depth)
;;                     (parents node-parents)
;;                     (loopy? node-loopy?))

;; (define* (make-node tree depth parents #:optional (loopy? #f))
;;   (%make-node tree depth (list->vector parents) loopy?))

;; (define (graph-ref grph pos)
;;   (array-ref grph (position-y pos) (position-x pos) (position-dir pos)))

;; (define (%graph-set! grph pos node)
;;   (array-set! grph node (position-y pos) (position-x pos) (position-dir pos)))

;; (define (%parent-list grph acc idx)
;;   (define plist (node-parents (car acc)))
;;   (if (< idx (vector-length plist))
;;       (%parent-list grph (cons (vector-ref plist idx) acc) (+ 1 idx))
;;       (reverse! acc)))

;; (define* (%child-node grph pnode #:optional (end-loop? #f))
;;   (make-node (node-tree pnode)
;;              (+ 1 (node-depth pnode))
;;              (%parent-list grph (list pnode) 0)
;;              (and (node-loopy? pnode) (not end-loop?))))

;; (define (%dfs-data! grph mp pos revpath tree)
;;   (if pos
;;       (let ((prev (graph-ref grph pos)))
;;         (case prev                      ; TODO Cleanup
;;           ((#f)
;;            (%graph-set! grph pos (make-node tree -1 '() #t))
;;            (%dfs-data! grph mp (step-guard mp pos) (cons pos revpath) tree))
;;           (else
;;            (when (= tree (node-tree prev))
;;              (%graph-set! grph pos 'start-loop))
;;            (values revpath
;;                    (if (null? revpath) prev (%child-node grph prev)))))) ; TODO Cleanup
;;       (values revpath (make-node tree -1 '() #f))))

;; (define (%dfs-place! grph revpath node)
;;   ;; (format #t "%dfs-place! ~a (~a,~a)" (length revpath) (node-tree node) (node-depth node))
;;   (define end-loop? (and (node-loopy? node)
;;                          (eq? 'start-loop (graph-ref grph (car revpath)))))
;;   (%graph-set! grph (car revpath) node)
;;   (unless (null? (cdr revpath))
;;     (%dfs-place! grph (cdr revpath) (%child-node grph node end-loop?))))

;; (define (map->graph mp)
;;   (define-values (h w) (map-dimensions mp))
;;   (define grph (make-array #f h w 4))
;;   (define (ensure-node! pos tree)
;;     (define-values (revpath root-node) (%dfs-data! grph mp pos '() tree))
;;     (unless (null? revpath) (%dfs-place! grph revpath root-node))
;;     (if (= tree (node-tree root-node)) (+ 1 tree) tree))
;;   (define* (fill-cell! x y tree #:optional (dir 0))
;;     (if (= dir 4)
;;         tree
;;         (fill-cell! x y (ensure-node! (make-position x y dir) tree) (+ 1 dir))))
;;   (values grph (rectangle-fold fill-cell! 0 0 w 0 h)))

;; (define (graph-in-bounds? grph pos)
;;   (array-in-bounds? grph (position-y pos) (position-x pos) (position-dir pos)))

;; (define (node-ancestor child n)
;;   (if (zero? n)
;;       child
;;       (let ((idx (- (integer-length n) 1)))
;;         (node-ancestor (vector-ref (node-parents child) idx)
;;                        (logand n (lognot (ash 1 idx)))))))

;; (define (node-ancestor? child ancestor)
;;   (and (= (node-tree child) (node-tree ancestor))
;;        (or (node-loopy? ancestor)
;;            (let ((diff (- (node-depth child) (node-depth ancestor))))
;;              (and (not (negative? diff))
;;                   (eq? ancestor (node-ancestor child diff)))))))

;; (define (ends-with-loop? node)
;;   (or (node-loopy? node)
;;       (let ((len (vector-length (node-parents node))))
;;         (and (> len 0)
;;              (ends-with-loop? (vector-ref (node-parents node) (- len 1)))))))

;; (define (would-loop? grph init block-x block-y)
;;   (define (position-redirection pos)
;;     (if (graph-in-bounds? grph pos)
;;         (let ((node (graph-ref grph pos)))
;;           (if (node? node)
;;               (cons node (graph-ref grph (position-rotate pos)))
;;               #f))
;;         #f))
;;   (define redirections
;;     (filter-map position-redirection
;;                 (list (make-position block-x (+ block-y 1) 0)
;;                       (make-position (- block-x 1) block-y 1)
;;                       (make-position block-x (- block-y 1) 2)
;;                       (make-position (+ block-x 1) block-y 3))))
;;   ;; (for-each (lambda (r) (display (cons (cons (node-tree (car r)) (node-depth (car r)))
;;   ;;                                      (cons (node-tree (cdr r)) (node-depth (cdr r))))))
;;   ;;           redirections)
;;   (let iter ((node init) (rounds-left 4))
;;     (define appliable-redirections
;;       (filter (lambda (red) (node-ancestor? node (car red))) redirections))
;;     (define (red< r1 r2)
;;       (< (modulo (- (node-depth node) (node-depth (car r1))) 1000000000)
;;          (modulo (- (node-depth node) (node-depth (car r2))) 1000000000)))
;;     (cond ((null? appliable-redirections) (ends-with-loop? node))
;;           ((= rounds-left 0) #t)
;;           (else (iter (cdar (sort appliable-redirections red<))
;;                       (- rounds-left 1))))))

;; (define* (part2 #:optional (port (current-input-port)))
;;   (define-values (mp initpos) (read-map port))
;;   (define-values (h w) (map-dimensions mp))
;;   (define-values (grph trees) (map->graph mp))
;;   (format #t "Node preprocessed successfully with ~a trees.\n" trees)
;;   ;;  (let ((x  (make-array '() h w 4))) (array-map! x (lambda (node) (vector-map (lambda (i n) (cons (node-tree n) (node-depth n))) (node-parents node))) grph) (display x))
;;   (define initnode (graph-ref grph initpos))
;;   (define (count-cell x y)
;;     (if (and (not (and (= x (position-x initpos)) (= y (position-y initpos))))
;;              (not (map-obstacle? mp x y))
;;              (would-loop? grph initnode x y))
;;         (begin (display (cons x y)) 1)
;;         0))
;;   (rectangle-fold (lambda (x y cnt) (+ cnt (count-cell x y))) 0 0 h 0 w))
;; ;;; (1949, 2343)
