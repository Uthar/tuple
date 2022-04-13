;;; An immutable, persistent vector data structure, not unlike Clojure's.

(in-package :tuple)

(declaim (optimize speed))

;;; API

(defgeneric tuple (&rest elems)
  (:documentation
   "Create a tuple containing elems"))

(defgeneric lookup (tuple index)
  (:documentation
   "Return element at index in log32N time"))

(defgeneric insert (tuple index val)
  (:documentation
   "Return a new tuple that has val at index and shares structure with the old one."))

(defgeneric conj (tuple val)
  (:documentation
   "Return a new, 1+ sized tuple with val at the back"))

(defgeneric count (tuple)
  (:documentation
   "Return, in constant time, the number of objects in a tuple"))

(declaim (ftype (function (tuple) (unsigned-byte 32)) count))

(defgeneric pop (tuple)
  (:documentation
   "Return a new, 1- sized tuple"))

(defgeneric slice (tuple start &optional end)
  (:documentation
   "Return a new tuple containing only items indexed from start to end.

    No lookups or insertions are performed in the process, so this
    operation is very fast."))

(defgeneric peek (tuple)
  (:documentation
   "Return, in constant time, the last element of tuple"))

(defgeneric equal (x y)
  (:documentation
   "Return true if two objects are equal.

Extension point for teaching tuples about new data types with
different equality semantics"))

(defgeneric concat (a b)
  (:documentation
   "Return the concatenation of a and b"))

(defstruct (tuple (:constructor nil) (:copier nil))
  "Parent type for internal tuple implementations, also the API type")

;;; Implementation follows

;; using structs provides a nice performance boost in SBCL: http://www.sbcl.org/manual/index.html#Efficiency

(defstruct (%node (:copier nil)
                  (:conc-name node-)
                  (:constructor make-node))
  "
A node contains an array of either other nodes or values, which forms
a tree - values being the leaves of it.
  "
  (array nil :type (simple-vector 32)))

(defstruct (%tuple (:copier nil)
                   (:conc-name tuple-)
                   (:constructor make-tuple)
                   (:include tuple))
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
new node only when it's full. Otherwise new values are inserted to the
tail.

Count is the number of items in the tuple, that is, the number of leaf
nodes plus the number of elements in the tail.
  "
  (shift  5 :type (integer 0 30)      :read-only t)
  (root nil :type %node               :read-only t)
  (tail nil :type (simple-vector 32)  :read-only t)
  (count  0 :type (unsigned-byte 32)  :read-only t))

(declaim (inline empty-node))

(defun empty-node ()
  (make-node
   :array
   (make-array 32 :initial-element nil)))

(declaim (inline empty-tail)
         (ftype (function () (simple-vector 32)) empty-tail))

(defun empty-tail ()
  (make-array 32 :initial-element nil))

(declaim (inline empty-tuple))

(defun empty-tuple ()
  (make-tuple
   :root (empty-node)
   :tail (empty-tail)
   :count 0
   :shift 5))

(declaim (inline copy-node))

;; (defun copy-node-array (node)
;;   (declare (optimize speed))
;;   (let ((array (node-array node))
;;         (new-array (make-array 32 :initial-contents
;;     (

(defun copy-node (node)
  (declare (optimize speed))
  (make-node :array (copy-seq (node-array node))))

(declaim (inline copy-tail)
         (ftype (function ((simple-vector 32)) (simple-vector 32)) copy-tail))

(defun copy-tail (tail)
  (declare (optimize speed))
  (copy-seq tail))

(declaim
 (inline nextid)
 (ftype (function ((unsigned-byte 32) &optional (integer 0 30)) (unsigned-byte 5)) nextid))

(defun nextid (index &optional (shift 0))
  (declare (optimize speed))
  (logand (ash index (- shift)) #b11111))

(declaim (inline tuple-should-grow-new-root?))

;; tuple-full-p
(defun tuple-should-grow-new-root? (tuple)
  (declare (optimize speed))
  (let ((count (tuple-count tuple))
        (shift (tuple-shift tuple)))
    (= count
       (+
       ;; tail is full
        32

        ;; tree is full
        (expt 2 (+ 5 shift))))))

(declaim (inline tuple-index-in-tail?))
(declaim
 (ftype (function (tuple (unsigned-byte 32)) boolean) tuple-index-in-tail?))

(defun tuple-index-in-tail? (tuple index)
  (declare (optimize speed))
  (let* ((count (tuple-count tuple))
         ;; Nodes are always full 32 arrays, so this gets the number of
         ;; elements that are outside the nodes, and so, are in the tail.
         (tail-count (1+ (mod (1- count) 32)))
         (threshold (- count tail-count)))
    (or (<= count 32)  ;; needed?
        (>= index threshold))))

(defmethod lookup ((tuple %tuple) index)
  (declare (optimize speed))
  (if (tuple-index-in-tail? tuple index)
      (svref (tuple-tail tuple) (nextid index))
      (loop :with shift := (tuple-shift tuple)
            :with root-array := (node-array (tuple-root tuple))
            :for level :downfrom shift :above 0 :by 5
            :for nextid := (nextid index level)
            :for arr := (node-array (svref root-array nextid))
            :then (node-array (svref arr nextid))
            :finally (return (svref arr (nextid index))))))

(define-symbol-macro next-node (svref (node-array node) nextid))

;; still 2x slower than clojure... but why?
(defmethod insert ((tuple %tuple) index val)
  (declare (optimize speed))
  (if (tuple-index-in-tail? tuple index)
      (let ((tail (copy-tail (tuple-tail tuple))))
        (setf (aref tail (nextid index)) val)
        ;; Can share everything else
        (make-tuple
         :root (tuple-root tuple)
         :shift (tuple-shift tuple)
         :count (tuple-count tuple)
         :tail tail))
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

(declaim (inline tuple-space-in-tail?))
;; tail-full-p
(defun tuple-space-in-tail? (tuple)
  ;; if count is a multiple of 32, then the tail is full, since it
  ;; gets flushed during every 33th conj
  (declare (optimize speed))
  (let ((count (tuple-count tuple)))
    (or (< count 32)
        (not (zerop (mod count 32))))))

(declaim (inline tuple-push-tail))
;; let  count
(defun tuple-push-tail (tuple val)
  (declare (optimize speed))
  (let ((tail (copy-tail (tuple-tail tuple))))
    (setf (svref tail (nextid (tuple-count tuple))) val)
    (make-tuple :shift (tuple-shift tuple)
                :count (1+ (tuple-count tuple))
                :root (tuple-root tuple)
                :tail tail)))

;; do something with the similiarities between the next two

;; FIXME could reuse tuple-grow-from-tail
(defun tuple-grow-share-root (tuple val)
  "Incorporate current tail into node tree, push val to a new tail, increasing tree depth"
  (let ((root (empty-node)))

    ;; share whole thing on the 'left'
    (setf (aref (node-array root) 0) (tuple-root tuple))

    (loop

          ;; tree and tail are full at this point

          ;; going down the last node (because of shift+5) - NOT the last value
          :with index fixnum := (1- (tuple-count tuple))

          ;; increase tree depth
          :with shift fixnum := (+ 5 (tuple-shift tuple))

          :for level fixnum :downfrom shift :above 0 :by 5
          :for nextid fixnum := (nextid index level)

          ;; make a new path to leaf
          :for node := (setf node root next-node (empty-node))
            :then (setf next-node (empty-node))

          :finally

             ;; place the full tail as the value array of the last node (leaves)
             ;; can use reference since everything else does CoW
             (setf (node-array node) (tuple-tail tuple))

             ;; place the incoming val in a fresh tail
             (let ((tail (empty-tail)))
               (setf (svref tail 0) val)

               (return (make-tuple
                        :root root
                        :shift shift
                        :count (1+ (tuple-count tuple))
                        :tail tail))))))

(declaim (inline make-tuple make-node tuple-grow-from-tail))

(defun tuple-grow-from-tail (tuple val)
  "Incorporate current tail into node tree, push val to a new tail"
  (loop :with index := (1- (tuple-count tuple))
        :with shift := (tuple-shift tuple)

        ;; Copy root, because we're making a new path to leaf
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
           (setf (svref tail 0) val)
           (setf (node-array node) (tuple-tail tuple))
           (return
             (make-tuple
              :root root
              :shift shift
              :count (1+ (tuple-count tuple))
              :tail tail))))

(defmethod conj ((tuple %tuple) val)
  (cond ((tuple-space-in-tail? tuple)
         (tuple-push-tail tuple val))
        ((tuple-should-grow-new-root? tuple)
         (tuple-grow-share-root tuple val))
        (t
         (tuple-grow-from-tail tuple val))))

(defmethod count ((tuple %tuple))
  (tuple-count tuple))

(defstruct (%slice (:conc-name slice-)
                   (:constructor make-slice)
                   (:include tuple))
  "
A narrowed view on a tuple, for efficient subvec.
Should support all the operations like a normal tuple
  "

  ;; The original tuple
  (tuple (empty-tuple) :type tuple :read-only t)

  ;; Where the view on the original tuple begins
  (start 0 :type (unsigned-byte 32) :read-only t)

  ;; Where it ends
  (end 0 :type (unsigned-byte 32) :read-only t))

(defmethod slice ((tuple %tuple) start &optional (end (count tuple)))
  (make-slice :start start :end end :tuple tuple))

;; conj on slice is increase end and insert there to the backing tuple
;; if reached end of backing tuple, conj on it instead
(defmethod conj ((tuple %slice) val)
  (let ((end (slice-end tuple))
        (backing-tuple (slice-tuple tuple)))
    (make-slice :start (slice-start tuple)
                :end (1+ end)
                :tuple
                (if (= end (tuple-count backing-tuple))
                    (conj   backing-tuple val)
                    (insert backing-tuple end val)))))

;; insert on slice is just insert on the backing tuple with index+start
(defmethod insert ((tuple %slice) index val)
  (let ((start (slice-start tuple)))
    (declare (type (unsigned-byte 32) start index))
    (make-slice :start start
                :end (slice-end tuple)
                :tuple (insert (slice-tuple tuple) (+ index start) val))))

(defmethod pop ((tuple tuple))
  (slice tuple 0 (1- (count tuple))))

;; subvec on slice is just make a slice with different start and end
(defmethod slice ((tuple %slice) start &optional (end (count tuple)))
  (let ((start* (slice-start tuple)))
    (declare (type (unsigned-byte 32) start end start*))
    (make-slice :start (+ start start*)
                :end (+ end start*)
                :tuple (slice-tuple tuple))))

;; count on slice is (- end start)
(defmethod count ((tuple %slice))
  (- (slice-end tuple) (slice-start tuple)))

;; lookup on slice is lookup on backing tuple with index+start
(defmethod lookup ((tuple %slice) index)
  (let ((start (slice-start tuple)))
    (declare (type (unsigned-byte 32) index start))
    (lookup (slice-tuple tuple) (+ index start))))

(defmethod peek ((tuple tuple))
  (lookup tuple (1- (count tuple))))

(defmethod tuple (&rest elems)
  "Create a tuple containing arbitrary elems"
  (cl:reduce 'conj elems :initial-value (empty-tuple)))

;; FIXME implement sequence protocol
(defun sequence->tuple (sequence)
  (cl:reduce 'conj sequence :initial-value (empty-tuple)))


;;; utility

;; these are probably sloooow

(defun map (fn tuple)
  (declare (type function fn))
  (sequence->tuple
   (loop for x fixnum below (count tuple)
         collect (funcall fn (lookup tuple x)))))

(defun filter (fn tuple)
  (declare (type function fn))
  (sequence->tuple
   (loop for i fixnum below (count tuple)
         for x fixnum = (lookup tuple i)
         if (funcall fn x)
           collect x)))

(defmethod tuple->list ((tuple tuple))
  (loop for x fixnum below (count tuple)
        collect (lookup tuple x)))

(defun reduce (fn tuple)
  (cl:reduce fn (tuple->list tuple)))

(defmethod equal ((val1 t) (val2 t))
  "Return true if two vals are cl:equal"
  (cl:equal val1 val2))

(defmethod equal ((tuple1 tuple) (tuple2 tuple))
  "Return true if two tuples are equal in size, and their elements, in order, are tuple:equal"
  (let ((cnt1 (count tuple1))
        (cnt2 (count tuple2)))
    (declare (type fixnum cnt1 cnt2))
    (and (= cnt1 cnt2)
         (loop for x fixnum below cnt1
               for y fixnum below cnt2
               always (equal (lookup tuple1 x) (lookup tuple2 y))))))

;; see seq:cat in http://fossil.galkowski.xyz/cl-fp
(defmethod concat ((a tuple) (b tuple))
  (loop with tuple = a
        for n fixnum below (count b)
        do (setf tuple (conj tuple (lookup b n)))
        finally (return tuple)))

;; is this too slow?
;; could just:
;;  (seq:cat (list x) tuple)
;; less consing
(defun tuple-cons (x tuple)
  (sequence->tuple
   (cons x (tuple->list tuple))))

(defmethod print-object ((object tuple) stream)
  (declare (type stream stream))
  (print-unreadable-object (object stream :type t)
    (loop :for i fixnum :below (count object)
          :do (format stream "~s " (lookup object i)))))
