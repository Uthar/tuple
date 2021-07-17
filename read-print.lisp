;; read-print support for tuples
(in-package :cl-user)

(defun tuple-reader (stream char)
  (declare (ignore char))
  (reduce #'conj `(,(empty-tuple)
                   ,@(mapcar #'eval (read-delimited-list #\] stream t)))))

(set-macro-character #\[ #'tuple-reader)
(set-macro-character #\] (get-macro-character #\) nil))

(defmethod print-object ((object tuple) stream)
  (format stream "[ ")
  (loop :for i :below (tuple-count object)
        :do (format stream "~s " (lookup object i)))
  (format stream "]"))
