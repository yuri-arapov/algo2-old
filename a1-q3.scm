;; a1-q3.scm
;;
;; Stanford ONLINE
;;
;; Algorithms: Design and Analysis, Part 2
;;
;; https://class.coursera.org/algo2-2012-001/quiz/attempt?quiz_id=75
;;
;; Programming Assignment #1
;; Question 3
;;
;; Yuri Arapov <yuridichesky@gmail.com>


(use-modules (srfi srfi-69)) ; hash tables


(load "algo2-utils.scm")
(load "heap.scm")


(define (make-edge node1 node2 cost)
  (list node1 node2 cost))

(define (edge-node1 edge) (first  edge))
(define (edge-node2 edge) (second edge))
(define (edge-cost  edge) (third  edge))

(define (edge< e1 e2) (< (edge-cost e1) (edge-cost e2)))


(define (random-graph-node edges)
  (edge-node1 (list-ref edges (random (length edges)))))


(define (parse-graph-header line)
  (apply values
         (map string->number (check-length 2 (line->words line " ")))))


(define (parse-edge line)
  (apply make-edge (map string->number (check-length 3 (line->words line " ")))))


(define (read-graph file)
  (call-with-input-file
    file
    (lambda (port)
      (let-values (
        ((total-nodes total-edges) (parse-graph-header (read-line port))))

        (let loop ((res  '())
                   (line (read-line port)))

          (if (eof-object? line)
            (if (= total-edges (length res)) (reverse res)
              (error (format #f "number of edges read from file (~d) does not match with header (~d)"
                             (length res) total-edges)))

            (let ((e (parse-edge line)))
              (if (> (edge-node1 e) total-nodes)
                (error (format #f "node index (~d) exceeds header limit (~d)"
                               (edge-node1 e) total-nodes)))
              (if (> (edge-node2 e) total-nodes)
                (error (format #f "node index (~d) exceeds header limit (~d)"
                               (edge-node1 2) total-nodes)))

              (loop (cons (parse-edge line) res)
                    (read-line port)))))))))


(define (graph-cost edges)
  (fold
    (lambda (e res) (+ res (edge-cost e)))
    0
    edges))


(define (prims-mst-bruteforce edges)

  (let* ((max-cost    (fold (lambda (e max-cost) (max max-cost (edge-cost e)))
                            (edge-cost (car edges))
                            (cdr edges)))
         (inf-cost    (1+ max-cost))
         (mst-nodes   (make-hash-table =)))

    (define (add-mst-node n)
      (hash-table-set! mst-nodes n #t))
    (define (mst-node? n)
      (hash-table-ref/default mst-nodes n #f))

    (define (crossing-edge? e)
      (not (equal? (mst-node? (edge-node1 e))
                   (mst-node? (edge-node2 e)))))

    (define (make-null-edge) (make-edge 0 0 inf-cost))
    (define (null-edge? e) (= (edge-cost e) inf-cost))

    (define (get-min-edge)
      (fold
        (lambda (e min-edge)
          (if (and (crossing-edge? e) (edge< e min-edge))
            e
            min-edge))
        (make-null-edge)
        edges))

    (add-mst-node (random-graph-node edges))

    (let loop ((mst-edges '()))
      (let ((min-edge (get-min-edge)))
        (if (null-edge? min-edge) mst-edges
          (begin
            (add-mst-node (edge-node1 min-edge))
            (add-mst-node (edge-node2 min-edge))
            (loop (cons min-edge mst-edges))))))))


(define (prims-mst-simple-heap edges)
  (let ((mst-nodes (make-hash-table =))
        (ranged-edges (make-heap (length edges) edge<)))

    (define (add-mst-node n)
      (hash-table-set! mst-nodes n #t))
    (define (mst-node? n)
      (hash-table-ref/default mst-nodes n #f))

    (define (crossing-edge? e)
      (not (equal? (mst-node? (edge-node1 e))
                   (mst-node? (edge-node2 e)))))

    (define (get-min-edge)
      (let loop ()
        (let ((e (ranged-edges 'get)))
          (cond ((not e) #f)
                ((not (crossing-edge? e)) (loop))
                (else e)))))

    (for-each (lambda (e) (ranged-edges 'add e)) edges)

    (add-mst-node (random-graph-node edges))

    (let loop ((mst-edges '()))
      (let ((min-edge (get-min-edge)))
        (if (not min-edge) mst-edges
          (begin
            (add-mst-node (edge-node1 min-edge))
            (add-mst-node (edge-node2 min-edge))
            (loop (cons min-edge mst-edges))))))))

;; end of file
;; vim: et
