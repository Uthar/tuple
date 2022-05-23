(in-package :tuple)

(defun range* (n)
  (loop for x below n collect (random n)))

(defmacro bench (&body body)
  `(progn
     (time ,@body)
     (sb-ext:gc :full t)))

;; (single core)

(defvar range (range* 1e6))

;; 1kk random append
(bench
  (tuple:count (cl:reduce #'append range :initial-value (tuple))))

(defparameter tup (cl:reduce #'append range :initial-value (tuple)))

;; 2kk random insert
;; (sb-sprof:with-profiling (:max-samples 1000
;;                           :report :flat
;;                           :loop nil)
(bench
  (dotimes (_ 2)
  (dotimes (n (floor 1e6))
    (insert tup n n))))

  ;; )

;; 1kkk lookups
(bench
  (dotimes (_ 10)
  (dotimes (n (floor 1e6))
    (lookup tup n))))
