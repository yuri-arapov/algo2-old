;; 
;; Median maintenance problem

(load "heap.scm")

(define *debug* #f)

(define (debug-print . args)
  (if *debug*
  (apply format #t args)
  #f))


;; make median maintenance worker
(define (make-median-maintenance size)
  ;; two heaps
  (let ((h-low (make-heap size >))   ;; keep track of max
        (h-high (make-heap size <))) ;; keep track of min

    ;; function that adds number to the worker and returns current median
    (lambda (n)
      ;; add number to either h-low or h-high
      (if (and (positive? (heap-count h-low)) (< n (heap-top h-low)))
          (heap-add h-low n)
          (heap-add h-high n))
      (debug-print "count ~a ~a\n" (heap-count h-low) (heap-count h-high))
      (debug-print "top ~a ~a\n" (heap-top h-low) (heap-top h-high))
      ;; rebalance h-low and h-high if necessary
      (if (> (abs (- (heap-count h-low) (heap-count h-high))) 1)
          (if (> (heap-count h-low) (heap-count h-high))
              (heap-add h-high (heap-get h-low))
              (heap-add h-low (heap-get h-high))))
      (debug-print "count ~a ~a\n" (heap-count h-low) (heap-count h-high))
      (debug-print "top ~a ~a\n" (heap-top h-low) (heap-top h-high))
      ;; get median
      (let* ((k (+ (heap-count h-low) (heap-count h-high)))
             (m (if (odd? k) (/ (+ k 1) 2) (/ k 2))))
        (debug-print "k ~a m ~a\n" k m)
        (if (= m (heap-count h-low))
            (heap-top h-low)       ;; either max of the mins
            (heap-top h-high)))))) ;; or min of the maxes


;; 1213
(define (problem)
  (let* ((data (read-file-with "Median.txt" string->number))
         (mm (make-median-maintenance (length data))))
    (remainder (apply + (map mm data)) 10000)))




;; end of file
