(use-package :tuple)

(defun range* (n)
  (loop for x below n collect (random n)))

(defmacro bench (&body body)
  `(progn
     (time ,@body)
     (sb-ext:gc :full t)))

;; as fast as clojure, which is unexpected
;; 1kkk random conj
(bench
  (reduce #'tuple-conj (range* 1e7) :initial-value (empty-tuple)))

(defparameter tup (reduce #'tuple-conj (range* 1e6) :initial-value (empty-tuple)))

;; 2x slower than clojure
;; 1kk random insert
(bench
  (dotimes (_ 10)
  (dotimes (n (floor 1e6))
    (tuple-insert tup n (random 100)))))

;; 1.5x slower than clojure, which is also unexpected
;; 1kkkk lookups
(bench
  (dotimes (n (floor 10e8))
    (tuple-lookup tup n)))
