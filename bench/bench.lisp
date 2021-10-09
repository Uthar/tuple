(in-package :tuple)

(defun range* (n)
  (loop for x below n collect (random n)))

(defmacro bench (&body body)
  `(progn
     (time ,@body)
     (trivial-garbage:gc :full t)))

;; (single core)

;; 1kk random conj
(bench
  (cl:reduce #'conj (range* 1e6) :initial-value (tuple)))

(defparameter tup (cl:reduce #'conj (range* 1e6) :initial-value (tuple)))

;; 2kk random insert
(bench
  (dotimes (_ 2)
  (dotimes (n (floor 1e6))
    (insert tup n (random 100)))))

;; 1kkk lookups
(bench
  (dotimes (n (floor 10e6))
    (lookup tup n)))
