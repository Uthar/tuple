;; (defpackage :kpg.util
;;   (:nicknames :kpg)
;;   (:use :cl :uiop)
;;   ;; (:import-from :alexandria :hash-table-alist)
;;   (:export
;;    #:->
;;    #:->>
;;    #:as->
;;    #:copy-directory
;;    #:defclass*
;;    #:doc
;;    #:fn
;;    #:for
;;    #:range
;;    #:range*
;;    #:take))

;; (declaim (optimize (debug 3)))

;; (in-package :cl)

;; (defun dictionary-reader (stream char)
;;   (declare (ignore char))
;;   (let ((list (read-delimited-list #\} stream t)))
;;     (assert (evenp (length list)))
;;     (apply #'cl-hamt:dict-insert `(,(cl-hamt:empty-dict) ,@list))))

;; (set-macro-character #\{ #'dictionary-reader)
;; (set-macro-character #\} (get-macro-character #\) nil))

;; (defun vector-reader (stream char)
;;   (declare (ignore char))
;;   (eval `(fset:seq ,@(read-delimited-list #\] stream t))))

;; (set-macro-character #\[ #'vector-reader)

;; (make-dispatch-macro-character #\@)

;; (defun hash-table-reader (stream char _)
;;   (declare (ignore char _))
;;   (let ((list (read-delimited-list #\} stream t)))
;;     (assert (evenp (length list)))
;;     (let ((keys (loop :for x :in list :by #'cddr :collect x))
;;           (vals (loop :for x :in (rest list) :by #'cddr :collect x)))
;;       (loop :for k :in keys
;;             :for v :in vals
;;             :with h := (make-hash-table :test 'equal)
;;             :do (setf (gethash k h) v)
;;             :finally (return h)))))

;; (set-dispatch-macro-character #\@ #\{ #'hash-table-reader)

;; (defun seq-reader (stream char _)
;;   (declare (ignore char _))
;;   (loop :for x :in (read-delimited-list #\] stream t)
;;         :with v := (make-array 10 :adjustable t :fill-pointer 0)
;;         :do (vector-push-extend x v)
;;         :finally (return v)))

;; (set-dispatch-macro-character #\@ #\[ #'seq-reader)
;; (set-macro-character #\] (get-macro-character #\) nil))

;; (defmethod print-object ((object cl-hamt:hash-dict) stream)
;;   (format stream "{ ")
;;   (loop :for kv :in (reverse (cl-hamt:dict->alist object))
;;         :do (format stream "~s ~s " (car kv) (cdr kv)))
;;   (format stream "}"))

;; (defmethod print-object ((object hash-table) stream)
;;   (format stream "@{ ")
;;   (loop :for kv :in (reverse (hash-table-alist object))
;;         :do (format stream "~s ~s " (car kv) (cdr kv)))
;;   (format stream "}"))

(defmacro fn (args &body body)
  `(lambda ,args ,@body))

(defmacro defclass* (name superclasses &rest slots)
  `(defclass ,name ,superclasses
     ,(mapcar (fn (slot) `(,slot :initarg ,slot :accessor ,slot)) slots)))

(defmacro ->> (&rest body)
  (reduce (fn (expansion form)
            (if (listp form)
                (append form (list expansion))
                (append (list form) (list expansion))))
          body))

(defmacro -> (&rest body)
  (reduce (fn (expansion form)
            (if (listp form)
                (append (list (first form)) (list expansion) (rest form))
                (append (list form) (list expansion))))
          body))

(defmacro as-> (&rest body)
  (let ((mark (second body))
        (body (append (list (first body)) (cddr body))))
    `(let* ,(mapcar (fn (form) `(,mark ,form)) (butlast body))
       ,@(last body))))

(defmacro for (exprs collect-expr)
  "List comprehension. Takes a list of exprs and one collect-expr
   Each expr being a list of a binding expr, list expr and 0 or more
   keyword exprs. Supported keywords are :when and :while, and they
   work like in the loop macro.

   Example:
       (for ((x '(A B C))
             (y '(1 2 3 4 5 6 7 8 9 0) :when (evenp y))
             (z '(x y)))
         (list x y z))
  "
  (reduce (fn (form expansion)
            (let* ((find-keyword
                     (lambda (keyword form)
                       (if (find keyword form)
                           (nth (1+ (position keyword form :from-end t)) form)
                           t)))
                   (when-form (funcall find-keyword :when form))
                   (while-form (funcall find-keyword :while form)))
              `(loop :for ,(first form) :in ,(second form) :while ,while-form :when ,when-form
                        ,@(if (null expansion)
                              `(:collect ,collect-expr)
                              `(:append ,expansion)))))
          exprs
          :initial-value '()
          :from-end t))

(defun doc (object)
  "Returns all kinds of documentation about object."
  (mapcar (fn (type) (documentation object type))
          '(function method-combination setf structure t type variable)))

(defun range (end)
  "Returns a list of numbers from 0 to end-1."
  (loop :for i :from 0 :below end :collect i))

(defun range* (start end &optional (step 1))
  "Returns a list of numbers from start to end-1, by step."
  (loop :for i :from start :below end :by step :collect i))

(defun take (n list)
  "Returns a list of the first n elements of list"
  (loop :repeat n :for x :in list :collect x))

;; (defun copy-directory (src dest)
;;   "Copy recursively directory SRC to DEST."
;;   (let* ((          src (-> src  ensure-directory-pathname truename))
;;          ( initial-dest (-> dest ensure-directory-pathname ensure-directories-exist truename))
;;          (prefix-length (-> src  pathname-directory length)))
;;     (labels
;;         ((dopath (path)
;;            (let ((dest (make-pathname
;;                         :directory (append (pathname-directory initial-dest)
;;                                            (-> path pathname-directory (subseq prefix-length)))
;;                         :defaults initial-dest)))
;;              (ensure-directories-exist dest)
;;              (if (directory-pathname-p path)
;;                  (mapcar #'dopath (append (directory-files path)
;;                                           (subdirectories path)))
;;                  (copy-file path
;;                             (make-pathname ; should take device from dest? for windows
;;                              :defaults path
;;                              :directory (pathname-directory dest))))
;;              dest)))
;;       (dopath src))))
