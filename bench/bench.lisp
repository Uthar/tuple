(in-package :tuple)

(defun range* (n)
  (loop for x below n collect (random n)))

(defmacro bench (&body body)
  `(progn
     (time ,@body)
     (sb-ext:gc :full t)))

(defvar range (range* 1e6))

(defvar a nil)

(bench
  (setf a (make-array 1000000 :initial-element 42)))

(bench
  (dotimes (_ 2)
  (dotimes (n (floor 1e6))
    (setf (aref a n) n))))

(bench
  (dotimes (_ 10)
  (dotimes (n (floor 1e6))
    (aref a n))))


(defvar tup nil)

;; 1kk random append
(bench
  (setf tup (cl:reduce #'append range :initial-value (tuple))))


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

