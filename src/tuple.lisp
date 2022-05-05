;;; An immutable, persistent vector data structure, not unlike Clojure's.

(in-package :tuple)

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


;; Utilities


;; FIXME implement sequence protocol
(defun sequence->tuple (sequence)
  (cl:reduce 'conj sequence :initial-value (tuple)))

(defun map (fn tuple)
  (sequence->tuple
   (loop for x below (count tuple)
         collect (funcall fn (lookup tuple x)))))

(defun filter (fn tuple)
  (sequence->tuple
   (loop for i below (count tuple)
         for x = (lookup tuple i)
         if (funcall fn x)
           collect x)))

(defmethod tuple->list ((tuple tuple))
  (loop for x below (count tuple)
        collect (lookup tuple x)))

(defun reduce (fn tuple)
  (cl:reduce fn (tuple->list tuple)))

(defmethod equal ((val1 t) (val2 t))
  "Return true if two vals are cl:equal"
  (cl:equal val1 val2))

(defmethod equal ((tuple1 tuple) (tuple2 tuple))
  "Return true if two tuples are equal in size, and their elements, in order, are tuple:equal"
  (and (= (count tuple1) (count tuple2))
       (loop for x below (count tuple1)
             for y below (count tuple2)
             always (equal (lookup tuple1 x) (lookup tuple2 y)))))

(defmethod concat ((a tuple) (b tuple))
  (loop with tuple = a
        for n below (count b)
        do (setf tuple (conj tuple (lookup b n)))
        finally (return tuple)))

;; is this too slow?
(defun tuple-cons (x tuple)
  (sequence->tuple
   (cons x (tuple->list tuple))))

(defmethod print-object ((object tuple) stream)
  (print-unreadable-object (object stream :type t)
    (loop :for i :below (count object)
          :do (format stream "~s " (lookup object i)))))

