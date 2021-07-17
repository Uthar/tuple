;; read-print support for tuples
(in-package :cl-user)

(defun tuple-reader (stream char)
  (declare (ignore char))
  `(funcall 'tuple ,@(read-delimited-list #\] stream t)))

(set-macro-character #\[ nil)
(set-macro-character #\] (get-macro-character #\) nil))

(defun dict-reader (stream char)
  (declare (ignore char))
  `(funcall 'dict ,@(read-delimited-list #\} stream t)))

(set-macro-character #\{ nil)
(set-macro-character #\} (get-macro-character #\) nil))

(defmethod print-object ((object tuple) stream)
  (print-unreadable-object (object stream :type t)
    (loop :for i :below (tuple-count object)
          :do (format stream "~s " (lookup object i)))))

(defmethod print-object ((object cl-hamt:hash-dict) stream)
  (print-unreadable-object (object stream :type t)
    (loop :for pair :in (cl-hamt:dict->alist object)
          :do (format stream "(~s ~s) " (car pair) (cdr pair)))))

(defun dict (&rest pairs)
  (assert (evenp (length pairs)))
  (loop for keyval = pairs then (rest (rest keyval))
        while (not (null keyval))
        for key = (first keyval)
        for val = (second keyval)
        for dict = (cl-hamt:dict-insert (cl-hamt:empty-dict) key val)
          then (cl-hamt:dict-insert dict key val)
        finally (return dict)))
