
;; true if given character c is a space
(define (space-char? c) (member c '(#\space #\tab #\newline #\return) char=?))


;; split string into list of strings according to given predicate pred
;; (pred c) -> true -- it is separator character
;; (pred c) -> false -- is is valid character
;; example:
;;   (split-string-p " a b c  d " space-char?) => ("a" "b" "c" "d")
(define (split-string-p str separator-pred)
  (define (add-to-res i j res)
    (if (> j i) (cons (substring str i j) res)
        res))
  (let ((end (string-length str)))
    (let loop ((i 0) (j 0) (res '()))
      (if (= j end)
          (reverse (add-to-res i j res))
          (if (separator-pred (string-ref str j))
              (loop (+ 1 j) (+ 1 j) (add-to-res i j res))
              (loop i (+ 1 j) res))))))


;; edge: head (ie vertex) and distance to it
(define (make-edge head dist) (cons head dist))
(define (edge-head e) (car e))
(define (edge-distance e) (cdr e))


;; turn string "123,43" into edge (123 . 43)
(define (string->edge str)
  (let ((head-dist (map string->number (string-split str #\,))))
    (if (null? head-dist) '()
        (make-edge (car head-dist) (cadr head-dist)))))


;; node: vertex and list of edges
(define (make-node vertex edges) (list vertex edges))
(define (node-vertex n) (car n))
(define (node-edges n) (cadr n))


;; parse sting "vertex space head,distance [ space [ head,distance ] ... ]"
;; into node
(define (parse-graph-line line)
  (let ((x (split-string-p line space-char?)))
    (make-node (string->number (car x))      ;; vertex
               (map string->edge (cdr x))))) ;; list of edges


;; read graph from file
(define (read-graph filename)
  (read-file-with filename parse-graph-line))


;; compute sum of all edges in the graph
(define (sum-all-paths graph)
  (fold (lambda (node res)
          (+ res (fold (lambda (edge res) (+ res (edge-distance edge)))
                       0 (node-edges node))))
        0
        graph))


;; Dijkstra shortest path (brute force)
;; g - graph as adjacency list
;; s - starting vertex
(define (dijkstra-shortest-graph-path g s)
  (let* (
     ;; +infinity
     (inf (+ 1 (sum-all-paths g)))

     ;; tracking of minimum paths
     (_a (make-vector (+ 1 (length g)) inf))
     (a (lambda (v) (vector-ref _a v)))
     (set-a (lambda (v socre) (vector-set! _a v socre)))
     (explored? (lambda (v) (< (a v) inf)))

     ;; score structure (distance from to)
     ;; "from" and "to" constitute an edge (tail and head)
     ;; "distance" - is a path distance from starting vertex to "to"
     (make-score (lambda (a from to) (list a from to)))
     (score-a (lambda (s) (car s)))
     (score-from (lambda (s) (cadr s)))
     (score-to (lambda (s) (caddr s)))

     ;; find edge with the best score that crosses the frontier.
     ;; return (score from to).
     (find-best-score
       (lambda ()
         (fold (lambda (node best-score)
                 (if (explored? (node-vertex node))
                     (let* ((from (node-vertex node))
                            (from-a (a from)))
                       (fold (lambda (edge best-score)
                               (let* ((w (edge-head edge))
                                      (d (edge-distance edge)))
                                 (if (and (not (explored? w))
                                          (< (+ from-a d) (score-a best-score)))
                                     (make-score (+ from-a d) from w)
                                     best-score)))
                             best-score
                             (node-edges node))) ;; throughout all node edges
                     best-score))
               (make-score inf 0 0)
               g)))) ;; throughout all graph nodes

    (set-a s 0) ;; mark starting node

    (dotimes (i (- (length g) 1))
              (let* ((bs (find-best-score))
                     (sa (score-a bs))
                     (from (score-from bs))
                     (to (score-to bs)))
                ;;(format #t "~a ~d ~d\n" i to sa)
                (set-a to sa)))

    _a)) ;; result is a vector of shortest path from s to every other nodes


;; 2599,2610,2947,2052,2367,2399,2029,2442,2505,3068
(define (problem)
  (let ((a (dijkstra-shortest-graph-path (read-graph "dijkstraData.txt") 1)))
    (map (lambda (n) (vector-ref a n)) '(7 37 59 82 99 115 133 165 188 197))))


(define (problem-pp)
  (string-concatenate (intersperse "," 1 (map number->string (problem)))))

;; end of file
;;
