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

(defclass tuple ()
  ((shift
    :initarg :shift
    :reader tuple-shift)
   (root
    :initarg :root
    :reader tuple-root)
   (count
    :initarg :count
    :reader tuple-count))
  (:documentation
   "
A tuple is an integer-indexed, immutable collection of items.

Shift is the number of bytes to shift left when descending down the
tree of nodes to find the next nodes' index.

Count is the number of items in the tuple, that is, the number of leaf
nodes.
   "))

(defun empty-node () (make-instance 'node
                                    :array
                                    (make-array 32 :initial-element nil)))

(defun empty-tuple () (make-instance 'tuple
                                     :root (empty-node)
                                     :count 0
                                     :shift 5))

(defun nextid (index &optional (shift 0))
  (logand (ash index (- shift)) #b11111))

(defun lookup (tuple index)
  (let ((arr (node-array (tuple-root tuple)))
        (shift (tuple-shift tuple)))
    (loop :for level :downfrom shift :above 0 :by 5
          :for nextid := (nextid index level)
          :do (setf arr (node-array (aref arr nextid))))
    (when arr
      (aref arr (nextid index)))))

(defun insert (tuple index val)
  (let* ((shift (tuple-shift tuple))
         (root (tuple-root tuple))
         (newroot (make-instance 'node :array (copy-seq (node-array root))))
         (newshift shift)
         (nextid nil)
         (node newroot)
         (arr (node-array newroot)))
    (loop :for level :downfrom shift :above 0 :by 5
          :do (setf nextid (nextid index level))
          ;; copy path from the root down the tree
          :do (setf (aref (node-array node) nextid)
                    (make-instance 'node
                                   :array
                                   (copy-seq (node-array (aref (node-array node) nextid)))))
          :do (setf node (aref (node-array node) nextid)) ;; set next node as current
          :do (setf arr (node-array node)))
    ;; same as when shift is 0
    (setf nextid (nextid index))
    (with-slots (array) node
      (setf array (copy-seq arr))
      (setf (aref array nextid) val))
    (make-instance 'tuple :root newroot :shift newshift :count (tuple-count tuple))))

(defun conj (tuple val)
  (let* ((index (tuple-count tuple)) ;; also the length of the new tuple
         (shift (tuple-shift tuple))
         (root (tuple-root tuple))
         (newroot (make-instance 'node :array (copy-seq (node-array root))))
         (newshift shift)
         (nextid nil)
         (node newroot)
         (arr (node-array newroot)))
    (cond
      ((zerop index)
       (let ((node (make-instance 'node :array (vector val)))
             (root (empty-node)))
         (setf (aref (node-array root) 0) node)
         (return-from conj (make-instance 'tuple :root root :shift 5 :count 1))))
      ((= index (expt 2 (+ 5 shift)))
       (let ((newroot (empty-node))
             (newshift (+ 5 shift)))
         (setf (aref (node-array newroot) 0) root) ;; share whole thing
         (setf node newroot)
         (loop :for level :downfrom newshift :above 5 :by 5
               :do (setf nextid (nextid index level))
               ;; but make a new path to leaf
               :do (setf (aref (node-array node) nextid) (empty-node))
               :do (setf node (aref (node-array node) nextid)))
         (setf (aref (node-array node) 0) (make-instance 'node :array (make-array 1)))
         (setf node (aref (node-array node) 0))
         (setf (aref (node-array node) 0) val)
         (make-instance 'tuple :root newroot :shift newshift :count (1+ index))))
      (t
       (progn
         (loop :for level :downfrom shift :above 0 :by 5
               :do (setf nextid (nextid index level))
               :if (null (aref (node-array node) nextid)) ;; copy the next node in the current node
                 :do (setf (aref (node-array node) nextid) (empty-node)) ;; assume 32 long nodes
               :do (setf (aref (node-array node) nextid)
                         (make-instance 'node
                                        :array
                                        (copy-seq (node-array (aref (node-array node) nextid)))))
               :do (setf node (aref (node-array node) nextid)) ;; set next node as current
               :do (setf arr (node-array node)))
         (with-slots (array) node
           (when (= 32 (length array)) ;; except for leafs
             (setf array (vector))
             (setf arr (vector)))
           (setf array
                 (make-array (1+ (length arr))
                             :initial-contents (concatenate 'vector arr (list val)))))
         (make-instance 'tuple :root newroot :shift newshift :count (1+ index)))))))

(defun tuple (&rest elems)
  (reduce #'conj elems :initial-value (empty-tuple)))


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
