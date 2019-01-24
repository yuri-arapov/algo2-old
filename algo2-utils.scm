;; algo2-utils.scm
;;
;; Stanford ONLINE
;;
;; Algorithms: Design and Analysis, Part 2
;;
;; Some code shared among programming assignments.
;;
;; Yuri Arapov <yuridichesky@gmail.com>


(use-modules (ice-9 rdelim)) ; delimited IO


(define (split-line line delim)
  (let loop ((res '())
             (line line))
    (let ((i (string-contains line delim)))
      (if (not i)
        (reverse (cons line res))
        (loop (cons (string-take line i) res) 
              (string-drop line (+ i (string-length delim))))))))


(define (trim-split s)
  (filter (lambda (e) (not (string=? "" e))) s))


(define (line->words line delim)
  (trim-split (split-line line delim)))


(define (check-length len ls)
  (if (= (length ls) len) ls
    (error (format #f "check-length failed: expected ~d elements, got ~d elements"
                   len (length ls)))))


;; end of file
;; vim: et
