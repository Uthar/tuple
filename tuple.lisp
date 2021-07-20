(in-package :cl-user)

(defclass node ()
  ((array
    :initarg :array
    :reader node-array))
  (:documentation
   "
A node contains an array of either other nodes or the tuple
values. Values exist on the tuple leaves.
   "))

(defclass tuple () ;; (standard-object sequence)
  ((shift
    :type (integer 0 30)
    :initarg :shift
    :reader tuple-shift)
   (root
    :initarg :root
    :reader tuple-root)
   (tail
    :initarg :tail
    :initform (make-array 32 :fill-pointer 0)
    :reader tuple-tail)
   (count
    :type (unsigned-byte 32)
    :initarg :count
    :reader tuple-count))
  (:documentation
   "
A tuple is an integer-indexed, immutable collection of items.

Shift is the number of bytes to start shifting the index by from when
descending down the tree of nodes to find the next nodes' index.

Count is the number of items in the tuple, that is, the number of leaf
nodes.
   "))

(declaim (ftype (function (tuple) (integer 0 30)) tuple-shift))
(declaim (ftype (function (tuple) (unsigned-byte 32)) tuple-count))
(declaim (ftype (function (tuple) (vector * 32)) tuple-tail))


;; used for node nodes
(defun empty-node () (make-instance 'node
                                    :array
                                    (make-array 32 :initial-element nil)))

;; used for value nodes
(defun single-node () (make-instance 'node
                                    :array
                                    (make-array 1 :initial-element nil)))

(defun zero-node () (make-instance 'node
                                    :array
                                    (vector)))

(defun empty-tuple () (make-instance 'tuple
                                     :root (empty-node)
                                     :count 0
                                     :shift 5))

(declaim
 (inline nextid)
 (ftype (function ((unsigned-byte 32) &optional (integer 0 30)) (unsigned-byte 5)) nextid))

(defun nextid (index &optional (shift 0))
  (declare (optimize speed))
  (logand (ash index (- shift)) #b11111))

(defun copy-node (node)
  (make-instance 'node :array (copy-seq (node-array node))))

(defun tuple-should-grow? (tuple)
  (let ((count (tuple-count tuple))
        (shift (tuple-shift tuple)))
    (= count (+ 32 (expt 2 (+ 5 shift))))))

(defun tuple-empty? (tuple)
  (zerop (tuple-count tuple)))

(defun tuple-index-in-tail? (tuple index)
  (let ((count (tuple-count tuple)))
    (or (< count 32)
        (>= index (- count (fill-pointer (tuple-tail tuple)))))))

(defun lookup (tuple index)
  (if (tuple-index-in-tail? tuple index)
      (aref (tuple-tail tuple) (nextid index))
      (loop :with shift := (tuple-shift tuple)
            :with root-array := (node-array (tuple-root tuple))
            :for level :downfrom shift :above 0 :by 5
            :for nextid := (nextid index level)
            :for arr := (node-array (aref root-array nextid))
              :then (node-array (aref arr nextid))
            ;; leafs are values, not nodes
            :finally (return (aref arr (nextid index))))))

(define-symbol-macro next-node (aref (node-array node) nextid))

(defun insert (tuple index val)
  (if (tuple-index-in-tail? tuple index)
      (let ((tuple (copy-tuple tuple)))
        (setf (aref (tuple-tail tuple) (nextid index)) val)
        tuple)
      (loop :with root := (copy-node (tuple-root tuple))
            :with shift := (tuple-shift tuple)
            :for level :downfrom shift :above 0 :by 5
            :for nextid := (nextid index level)
            ;; copy path from the root down the tree
            :for node := (setf node root next-node (copy-node next-node))
              :then (setf next-node (copy-node next-node))
            :finally
               (setf (aref (node-array node) (nextid index)) val)
               (let ((tuple (copy-tuple tuple)))
                 (setf (slot-value tuple 'root) root)
                 (return tuple)))))

(defun single-tuple (val)
  (let ((tail (make-array 32 :fill-pointer 0)))
    (vector-push val tail)
    (make-instance 'tuple :root (empty-node) :shift 5 :count 1 :tail tail)))

(defun tuple-space-in-tail? (tuple)
  (< (fill-pointer (tuple-tail tuple)) 32))

(defun copy-tuple (tuple)
  (let ((tail (tuple-tail tuple))
        (newtail (empty-tail)))
    (loop for x across tail do (vector-push x newtail))
    (make-instance 'tuple
                   :count (tuple-count tuple)
                   :tail newtail
                   :root (copy-node (tuple-root tuple))
                   :shift (tuple-shift tuple))))

(defun tuple-push-tail (tuple val)
  (let ((tuple (copy-tuple tuple)))
    (vector-push val (tuple-tail tuple))
    (incf (slot-value tuple 'count))
    tuple))

(defun empty-tail ()
  (make-array 32 :fill-pointer 0 :initial-element nil))

(defun tuple-grow-share-root (tuple val)
  (let ((root (empty-node)))
    (setf (aref (node-array root) 0) (tuple-root tuple)) ;; share whole thing on the 'left'
    (loop :with index := (1- (tuple-count tuple))
          :with shift := (+ 5 (tuple-shift tuple))
          :for level :downfrom shift :above 5 :by 5
          :for nextid := (nextid index level)
          ;; but make a new path to leaf
          :for node := (setf node root next-node (empty-node))
            :then (setf next-node (empty-node))
          :finally
             (setf (aref (node-array node) 0) (empty-node)
                   node (aref (node-array node) 0)
                   (slot-value node 'array) (copy-seq (tuple-tail tuple))) ;; insert tail here ?
             (let ((tail (empty-tail)))
               (vector-push val tail)
               (return (make-instance 'tuple :root root :shift shift :count (+ 2 index) :tail tail))))))

(defun tuple-push-val (tuple val)
  (let* ((index (tuple-count tuple)) ;; also the length of the new tuple
         (shift (tuple-shift tuple))
         (root (copy-node (tuple-root tuple)))
         (node root))
    (loop :for level :downfrom shift :above 0 :by 5
          :for nextid := (nextid index level)
          ;; ugh
          :do (setf next-node (if next-node ;; originally these are nil
                                  (copy-node next-node)
                                  (if (= 5 level)
                                      (zero-node)
                                      (empty-node)))
                    node next-node)
          :finally
             (return
               (with-slots (array) node
                 (setf array
                       (make-array (1+ (length array))
                                   :initial-contents (concatenate 'vector array (vector val))))
                 (make-instance 'tuple :root root :shift shift :count (1+ index)))))))

(defun tuple-grow-from-tail (tuple val)
  (let* ((index (1- (tuple-count tuple)))
         (shift (tuple-shift tuple))
         (root (copy-node (tuple-root tuple)))
         (node root)
         (tail (make-array 32 :fill-pointer 0)))
    (loop :for level :downfrom shift :above 0 :by 5
          :for nextid := (nextid index level)
          :do (setf next-node (if next-node
                                  (copy-node next-node)
                                  (if (= 5 level)
                                      (zero-node)
                                      (empty-node)))
                    node next-node)
          :finally
             (vector-push val tail)
             (setf (slot-value node 'array) (copy-seq (tuple-tail tuple)))
             (return
               (make-instance 'tuple
                              :root root
                              :shift shift
                              :count (+ 2 index)
                              :tail tail)))))

(defun conj (tuple val)
  (cond ((tuple-empty? tuple)
         (single-tuple val))
        ((tuple-space-in-tail? tuple)
         (tuple-push-tail tuple val))
        ((tuple-should-grow? tuple)
         (tuple-grow-share-root tuple val))
        (t
         (tuple-grow-from-tail tuple val))))

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
