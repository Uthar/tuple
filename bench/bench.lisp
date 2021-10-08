(in-package :tuple)

(defun range* (n)
  (loop for x below n collect (random n)))

(defmacro bench (&body body)
  `(progn
     (time ,@body)
     (trivial-garbage:gc :full t)))

;; (single core)

;; as fast as clojure, which is unexpected
;; 1kkk random conj
(bench
  (reduce #'conj (range* 1e6) :initial-value (tuple)))

(defparameter tup (reduce #'conj (range* 1e6) :initial-value (tuple)))

;; 2x slower than clojure
;; 1kk random insert
(bench
  (dotimes (_ 2)
  (dotimes (n (floor 1e6))
    (insert tup n (random 100)))))

;; 1.5x slower than clojure, which is also unexpected
;; 1kkkk lookups
(bench
  (dotimes (n (floor 10e6))
    (lookup tup n)))
