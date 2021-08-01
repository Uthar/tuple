(in-package :tuple)

(defstruct (node (:copier nil))
  "
A node contains an array of either other nodes or values, which forms
a tree - values being the leaves of it.
  "
  (array nil :type (simple-vector 32)))

(defstruct (tuple (:copier nil))
  "
A tuple is an integer-indexed, immutable collection of any kind of
object.

Shift is the number of bytes to start shifting the 32-bit index by
from when descending down the tree of nodes. To find the next nodes'
index, take the last 5 bits of the shifted number.

Tail is a vector containing the last 32 elements of the tuple. Insert,
lookup and conj is faster with it because there's no need to traverse
the tree for values in the tail.

Nodes are always (simple-vector 32) because the tail gets copied to a
new node only when it's full. Otherwise new values are vector-pushed
to the tail.

Count is the number of items in the tuple, that is, the number of leaf
nodes plus the fill-pointer of the tail.
  "
  (shift  5 :type (integer 0 30))
  (root nil :type node)
  (tail nil :type (vector t 32))
  (count  0 :type (unsigned-byte 32)))

(defun empty-node ()
  (make-node
   :array
   (make-array 32 :initial-element nil)))

(defun empty-tail ()
  (make-array 32 :fill-pointer 0 :initial-element nil))

(defun empty-tuple ()
  (make-tuple
   :root (empty-node)
   :tail (empty-tail)
   :count 0
   :shift 5))

(declaim (inline copy-node))

(defun copy-node (node)
  (declare (optimize speed (space 0) (debug 0) (safety 0) (compilation-speed 0)))
  (make-node :array (copy-seq (node-array node))))

(defun copy-tail (tail)
  (loop :with new := (empty-tail)
        :for x :across tail :do (vector-push x new)
        :finally (return new)))

(defun copy-tuple (tuple)
  (make-tuple
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

;; unused
(defun tuple-empty? (tuple)
  (zerop (tuple-count tuple)))

(declaim (inline tuple-index-in-tail?)
         (ftype (function (tuple (unsigned-byte 32)) boolean) tuple-index-in-tail?))

(defun tuple-index-in-tail? (tuple index)
  (declare (optimize speed))
  (let ((count (tuple-count tuple)))
    (or (< count 32) ;; index?
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

(declaim (ftype (function (tuple (unsigned-byte 32) t) tuple) tuple-insert))

;; still 2.5x slower than clojure... but why?
(defun tuple-insert (tuple index val)
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0) (compilation-speed 0)))
  (if (tuple-index-in-tail? tuple index)
      ;; FIXME
      ;; only need to  copy the tail here
      ;; i.e. dont need to copy root !
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
               (return (make-tuple :shift shift
                                   :root root
                                   :tail (tuple-tail tuple)
                                   :count (tuple-count tuple))))))

(defun tuple-space-in-tail? (tuple)
  (< (fill-pointer (tuple-tail tuple)) 32))

(defun tuple-push-tail (tuple val)
  ;; FIXME
  ;; only need to  copy the tail here
  ;; i.e. dont need to copy root !
  (let ((tuple (copy-tuple tuple)))
    (vector-push val (tuple-tail tuple))
    (incf (tuple-count tuple))
    tuple))

;; do something with the similiarities between the next two

(defun tuple-grow-share-root (tuple val)
  (let ((root (empty-node)))
    ;; share whole thing on the 'left'
    (setf (aref (node-array root) 0) (tuple-root tuple))

    ;; 1- cuz tail is shared there, val is put in new tail
    (loop :with index := (1- (tuple-count tuple))
          :with shift := (+ 5 (tuple-shift tuple))
          :for level :downfrom shift :above 0 :by 5
          :for nextid := (nextid index level)

          ;; make a new path to leaf
          :for node := (setf node root next-node (empty-node))
            :then (setf next-node (empty-node))

          :finally
             (setf (node-array node) (copy-seq (tuple-tail tuple)))
             (let ((tail (empty-tail)))
               (vector-push val tail)
               (return (make-tuple
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
           ;; FIXME could be faster? try changing node-array to (vector t 32)
           ;; could have been the same vector, but has to be a
           ;; simple-vector so copy-seq does just that
           (setf (node-array node) (copy-seq (tuple-tail tuple)))
           (return
             (make-tuple
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

;; these are probably slow as fuck

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

;; NOTE: equalp works too
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
