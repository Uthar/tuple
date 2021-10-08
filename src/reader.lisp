;; [1 2 (+ 3 4) 5] syntax

(defun tuple-reader (stream char)
  (declare (ignore char))
  `(tuple:tuple ,@(read-delimited-list #\] stream t)))

(set-macro-character #\[ 'tuple-reader)
(set-macro-character #\] (get-macro-character #\) nil))
