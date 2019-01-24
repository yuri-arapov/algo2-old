;; a1-q1-q2.scm
;;
;; Stanford ONLINE
;;
;; Algorithms: Design and Analysis, Part 2
;;
;; https://class.coursera.org/algo2-2012-001/quiz/attempt?quiz_id=75
;;
;; Programming Assignment #1
;; Question 1
;; Question 2
;;
;; Yuri Arapov <yuridichesky@gmail.com>


(load "algo2-utils.scm")


(define (make-job w l) (cons w l))
(define job-weight car)
(define job-length cdr)


(define (parse-job job)
  (apply make-job (map string->number (line->words job " "))))


(define (job-diff job)
  (- (job-weight job) (job-length job)))

(define (job-diff> j1 j2) ; true if j1 > j2
  (let ((diff (- (job-diff j1) (job-diff j2))))
    (cond ((positive? diff) #t)
          ((negative? diff) #f)
          (else (> (job-weight j1) (job-weight j2))))))


(define (job-ratio job)
  (/ (job-weight job) (job-length job)))

(define (job-ratio> j1 j2) ; true if j1 > j2
  (> (job-ratio j1) (job-ratio j2)))


(define (read-jobs file)
  (call-with-input-file 
    file
    (lambda (port)
      (let ((total (string->number (read-line port))))
        (let loop ((n    0)
                   (res  '())
                   (line (read-line port)))
          (if (eof-object? line)
            (if (not (= n total))
              (error (format #f "error reading jobs from ~s: ~d jobs expected, ~d jobs read\n"
                             file total n))
              (reverse res))
            (loop (1+ n) (cons (parse-job line) res) (read-line port))))))))


(define (jobs-weighted-completion-time jobs)
  (let loop ((res          0)
             (length-sofar 0)
             (jobs         jobs))
    (if (null? jobs) res
      (let* ((job (car jobs))
             (w   (job-weight job))
             (l   (job-length job))
             (ll  (+ length-sofar l)))
        (loop (+ res (* w ll)) ll (cdr jobs))))))



(define (q job-compare-proc)
  (jobs-weighted-completion-time 
    (sort (read-jobs "jobs.txt") job-compare-proc)))


;; Question 1
(define (q1)
  (q job-diff>))


;; Question 2
(define (q2)
  (q job-ratio>))


;; end of file
;; vim: et
