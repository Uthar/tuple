(in-package :cl-user)

(defclass node ()
  ((array
    :initarg :array
    :reader node-array))
  (:documentation
   "
A node contains an array of either other nodes or the tuple
values. Values exist on the tuple leaves, when tuple-shift is 0.
   "))

(defclass tuple () ;; (standard-object sequence)
  ((shift
    :initarg :shift
    :reader tuple-shift)
   (root
    :initarg :root
    :reader tuple-root)
   (tail
    :initarg :tail
    :initform (make-array 32 :initial-element nil)
    :reader tuple-tail)
   (count
    :initarg :count
    :reader tuple-count))
  (:documentation
   "
A tuple is an integer-indexed, immutable collection of items.

Shift is the number of bytes to shift the index by when descending
down the tree of nodes to find the next nodes' index.

Count is the number of items in the tuple, that is, the number of leaf
nodes. (?)
   "))

(defun empty-node () (make-instance 'node
                                    :array
                                    (make-array 32 :initial-element nil)))

(defun single-node () (make-instance 'node
                                    :array
                                    (make-array 1 :initial-element nil)))

(defun empty-tuple () (make-instance 'tuple
                                     :root (empty-node)
                                     :count 0
                                     :shift 5))

(declaim (inline nextid)
         (ftype (function (fixnum &optional (unsigned-byte 6)) fixnum) nextid))
(defun nextid (index &optional (shift 0))
  (declare (optimize speed))
  (logand (ash index (- shift)) #b11111))

(defun copy-node (node)
  (make-instance 'node :array (copy-seq (node-array node))))

(defun tuple-next-level? (tuple)
  (with-slots (count shift) tuple
    (= count (expt 2 (+ 5 shift)))))

(defun tuple-empty? (tuple)
  (zerop (tuple-count tuple)))

(defun lookup (tuple index)
  (loop :with shift := (tuple-shift tuple)
        :with root-array := (node-array (tuple-root tuple))
        :for level :downfrom shift :above 0 :by 5
        :for nextid := (nextid index level)
        :for arr := (node-array (aref root-array nextid)) :then (node-array (aref arr nextid))
        ;; leafs are values, not nodes
        :finally (return
                   (when arr (aref arr (nextid index))))))

(define-symbol-macro next-node (aref (node-array node) nextid))

(defun insert (tuple index val)
  (loop :with root := (copy-node (tuple-root tuple))
        :with shift := (tuple-shift tuple)
        :for level :downfrom shift :above 0 :by 5
        :for nextid := (nextid index level)
        ;; copy path from the root down the tree
        :for node := (setf node root next-node (copy-node next-node))
          :then (setf next-node (copy-node next-node))
        :finally
           (setf (aref (node-array node) (nextid index)) val)
           (return (make-instance 'tuple :root root :shift shift :count (tuple-count tuple)))))

(defun single-tuple (val)
  (let ((node (make-instance 'node :array (vector val)))
        (root (empty-node)))
    (setf (aref (node-array root) 0) node)
    (make-instance 'tuple :root root :shift 5 :count 1)))

(defun conj-share-root (tuple val)
  (let ((root (empty-node)))
    (setf (aref (node-array root) 0) (tuple-root tuple)) ;; share whole thing on the 'left'
    (loop :with index := (tuple-count tuple)
          :with shift := (+ 5 (tuple-shift tuple))
          :for level :downfrom shift :above 5 :by 5
          :for nextid := (nextid index level)
          ;; but make a new path to leaf
          :for node := (setf node root next-node (empty-node)) :then (setf next-node (empty-node))
          :finally
             (setf (aref (node-array node) 0) (single-node)
                   node (aref (node-array node) 0)
                   (aref (node-array node) 0) val)
             (return (make-instance 'tuple :root root :shift shift :count (1+ index))))))

(defun tuple-push-val (tuple val)
  (let* ((index (tuple-count tuple)) ;; also the length of the new tuple
         (shift (tuple-shift tuple))
         (root (copy-node (tuple-root tuple)))
         (node root))
    (loop :for level :downfrom shift :above 0 :by 5
          :for nextid := (nextid index level)
          :do (setf next-node (if next-node (copy-node next-node) (if (= 5 level) (zero-node) (empty-node)))
                    node next-node)
          :finally
             (return
               (with-slots (array) node
                 (setf array
                       (make-array (1+ (length array))
                                   :initial-contents (concatenate 'vector array (vector val))))
                 (make-instance 'tuple :root root :shift shift :count (1+ index)))))))

(defun conj (tuple val)
  (cond ((tuple-empty? tuple) (single-tuple val))
        ;; only this should create a new path
        ;; otherwise just push to a tail vector
        ((tuple-next-level? tuple) (conj-share-root tuple val))
        ;; otherwise just push the new val to the last node
        ;; FIXME: replace with insert to tail vector
        (t (tuple-push-val tuple val))))

(defun tuple (&rest elems)
  (reduce 'conj elems :initial-value (empty-tuple)))

;; FIXME implement sequence protocol
(defun sequence->tuple (sequence)
  (reduce 'conj sequence :initial-value (empty-tuple)))


;;; utility


(defun map* (fn tuple)
  (reduce #'conj
          `(,(empty-tuple)
            ,@(loop for x below (tuple-count tuple)
                    collect (funcall fn (lookup tuple x))))))

(defun filter (fn tuple)
  (reduce #'conj
          `(,(empty-tuple)
            ,@(loop for i below (tuple-count tuple)
                    for x = (lookup tuple i)
                    if (funcall fn x)
                    collect x))))

;; is this too slow?
(defun cons* (x tuple)
  (reduce #'conj `(,(conj (empty-tuple) x)
                   ,@(mapcar (lambda (x) (lookup tuple x))
                             (loop for i below (tuple-count tuple) collect i)))))
