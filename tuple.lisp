(in-package :tuple)

(declaim (optimize speed))

(defclass node ()
  ((array
    :initarg :array
    :reader node-array))
  (:documentation
   "
A node contains an array of either other nodes or values, values being
the leaves of such a tree.
   "))

(declaim (ftype (function (node) (simple-vector 32)) node-shift))

(defclass tuple () ;; (standard-object sequence)
  ((shift
    :type (integer 0 30)
    :initarg :shift
    :reader tuple-shift)
   (root
    :initarg :root
    :reader tuple-root)
   (tail
    :type (vector * 32)
    :initarg :tail
    :reader tuple-tail)
   (count
    :type (unsigned-byte 32)
    :initarg :count
    :reader tuple-count))
  (:documentation
   "
A tuple is an integer-indexed, immutable collection of any kind of
object.

Shift is the number of bytes to start shifting the 32-bit index by
from when descending down the tree of nodes to find the next nodes'
index in a nodes' array.

Tail is a vector containing the last 32 elements of the tuple. Insert,
lookup and conj is faster with it because there's no need to traverse
the tree for values in the tail.

Nodes are always (simple-vector 32) because the tail gets copied to a
new node when it's full.

Count is the number of items in the tuple, that is, the number of leaf
nodes plus the fill-pointer of the tail.
   "))

(declaim (ftype (function (tuple) (integer 0 30)) tuple-shift))
(declaim (ftype (function (tuple) (unsigned-byte 32)) tuple-count))
(declaim (ftype (function (tuple) (vector * 32)) tuple-tail))


(defun empty-node ()
  (make-instance 'node
                 :array
                 (make-array 32 :initial-element nil)))

(defun empty-tail ()
  (make-array 32 :fill-pointer 0 :initial-element nil))

(defun empty-tuple ()
  (make-instance 'tuple
                 :root (empty-node)
                 :tail (empty-tail)
                 :count 0
                 :shift 5))

(defun copy-node (node)
  (make-instance 'node :array (copy-seq (node-array node))))

(defun copy-tail (tail)
  (loop :with new := (empty-tail)
        :for x :across tail :do (vector-push x new)
        :finally (return new)))

(defun copy-tuple (tuple)
  (make-instance 'tuple
                 :count (tuple-count tuple)
                 :tail (copy-tail (tuple-tail tuple))
                 :root (copy-node (tuple-root tuple))
                 :shift (tuple-shift tuple)))


(declaim
 (inline nextid)
 (ftype (function ((unsigned-byte 32) &optional (integer 0 30)) (unsigned-byte 5)) nextid))

(defun nextid (index &optional (shift 0))
  (declare (optimize speed))
  (logand (ash index (- shift)) #b11111))

(defun tuple-should-grow-new-root? (tuple)
  (let ((count (tuple-count tuple))
        (shift (tuple-shift tuple)))
    (= count (+ 32 (expt 2 (+ 5 shift))))))

(defun tuple-empty? (tuple)
  (zerop (tuple-count tuple)))

(defun tuple-index-in-tail? (tuple index)
  (let ((count (tuple-count tuple)))
    (declare (type (unsigned-byte 32) index))
    (or (< count 32)
        (>= index (- count (fill-pointer (tuple-tail tuple)))))))

(defun tuple-lookup (tuple index)
  (if (tuple-index-in-tail? tuple index)
      (aref (tuple-tail tuple) (nextid index))
      (loop :with shift := (tuple-shift tuple)
            :with root-array := (node-array (tuple-root tuple))
            :for level :downfrom shift :above 0 :by 5
            :for nextid := (nextid index level)
            :for arr := (node-array (aref root-array nextid))
              :then (node-array (aref arr nextid))
            :finally (return (aref arr (nextid index))))))

(define-symbol-macro next-node (aref (node-array node) nextid))

(defun tuple-insert (tuple index val)
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

(defun tuple-space-in-tail? (tuple)
  (< (fill-pointer (tuple-tail tuple)) 32))

(defun tuple-push-tail (tuple val)
  (let ((tuple (copy-tuple tuple)))
    (vector-push val (tuple-tail tuple))
    (incf (slot-value tuple 'count))
    tuple))

(defun tuple-grow-share-root (tuple val)
  (let ((root (empty-node)))
    ;; share whole thing on the 'left'
    (setf (aref (node-array root) 0) (tuple-root tuple))

    ;; 1- cuz tail is shared, val is put in new tail
    (loop :with index := (1- (tuple-count tuple))
          :with shift := (+ 5 (tuple-shift tuple))
          :for level :downfrom shift :above 0 :by 5
          :for nextid := (nextid index level)

          ;; make a new path to leaf
          :for node := (setf node root next-node (empty-node))
            :then (setf next-node (empty-node))

          :finally
             (setf (slot-value node 'array) (copy-seq (tuple-tail tuple)))
             (let ((tail (empty-tail)))
               (vector-push val tail)
               (return (make-instance 'tuple
                                      :root root
                                      :shift shift
                                      :count (1+ (tuple-count tuple))
                                      :tail tail))))))

(defun tuple-grow-from-tail (tuple val)
  (loop :with index := (1- (tuple-count tuple))
        :with shift := (tuple-shift tuple)
        :with root := (copy-node (tuple-root tuple))
        :with tail := (empty-tail)
        :with node := root
        :for level :downfrom shift :above 0 :by 5
        :for nextid := (nextid index level)
        :do (setf next-node
                  (if next-node
                      (copy-node next-node)
                      (empty-node))
                  node next-node)
        :finally
           (vector-push val tail)
           (setf (slot-value node 'array) (copy-seq (tuple-tail tuple)))
           (return
             (make-instance 'tuple
                            :root root
                            :shift shift
                            :count (1+ (tuple-count tuple))
                            :tail tail))))

(defun tuple-conj (tuple val)
  (cond ((tuple-space-in-tail? tuple)
         (tuple-push-tail tuple val))
        ((tuple-should-grow-new-root? tuple)
         (tuple-grow-share-root tuple val))
        (t
         (tuple-grow-from-tail tuple val))))

(defun tuple (&rest elems)
  (reduce 'tuple-conj elems :initial-value (empty-tuple)))

;; FIXME implement sequence protocol
(defun sequence->tuple (sequence)
  (reduce 'tuple-conj sequence :initial-value (empty-tuple)))


;;; utility

(defun tuple-size (tuple)
  (tuple-count tuple))

(defun tuple-map (fn tuple)
  (sequence->tuple
   (loop for x below (tuple-count tuple)
         collect (funcall fn (tuple-lookup tuple x)))))

(defun tuple-filter (fn tuple)
  (sequence->tuple
   (loop for i below (tuple-count tuple)
         for x = (tuple-lookup tuple i)
         if (funcall fn x)
           collect x)))

(defun tuple->list (tuple)
  (loop for x below (tuple-size tuple)
        collect (tuple-lookup tuple x)))

(defun tuple-reduce (fn tuple)
  (reduce fn (tuple->list tuple)))

(defun tuple-eq (tuple1 tuple2)
  (and (= (tuple-size tuple1) (tuple-size tuple2))
       (loop for x in (tuple->list tuple1)
             for y in (tuple->list tuple2)
             always (funcall
                     (if (typep x 'tuple)
                         'tuple-eq
                         'equal)
                     x y))))

;; is this too slow?
(defun tuple-cons (x tuple)
  (sequence->tuple
   (cons x (tuple->list tuple))))

(defmethod print-object ((object tuple) stream)
  (print-unreadable-object (object stream :type t)
    (loop :for i :below (tuple-count object)
          :do (format stream "~s " (tuple-lookup object i))
          :if (= i 30)
            :do (format stream "... ")
                (return))))
