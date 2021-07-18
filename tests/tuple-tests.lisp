(ql:quickload :fiveam)
(ql:quickload :alexandria)

(in-package :cl-user)

(use-package :5am)

(test empty-tuple
  (let* ((tuple (empty-tuple)))
    (is (eq (tuple-count tuple) 0))))

(test 1-tuple
  (let* ((tuple (conj (empty-tuple) "foo")))
    (is (eq (tuple-count tuple) 1))
    (is (equal (lookup tuple 0) "foo"))))

(test 100k-tuple
  (let* ((tuple (reduce 'conj `(,(empty-tuple) ,@(alexandria:iota 100000)))))
    (is (eq (tuple-count tuple) 100000))
    (is (loop for n below 100000 always (eq (lookup tuple n) n)))))

(test immutable
  (let* ((tup1 (reduce 'conj '("foo" "bar") :initial-value (empty-tuple)))
         (tup2 (reduce 'conj '("baz" "quux") :initial-value tup1)))
    (insert tup2 0 :x)
    (is (equal (lookup tup1 0) "foo"))
    (is (equal (lookup tup2 0) "foo"))
    (insert tup2 1 :y)
    (is (equal (lookup tup1 1) "bar"))
    (is (equal (lookup tup2 1) "bar"))
    (is (equal (lookup tup2 2) "baz"))
    (is (equal (lookup tup2 3) "quux"))))

(test 100k-insert
  (let* ((tuple (reduce 'conj `(,(empty-tuple) ,@(alexandria:iota 10000))))
         (vals (make-array 10000
                           :initial-contents (loop for n below 10000 collect (funcall (gen-string))))))
    (is (eq (tuple-count tuple) 10000))
    (is (loop for n below 10000
              for tup = (insert tuple n (aref vals n)) then (insert tup n (aref vals n))
              always (loop for x below n always (equal (lookup tup x) (aref vals x)))
              always (loop for x from (1+ n) below 10000 always (equal (lookup tup x) x))))))


(run!)

;; (setf 10m-tuple (sequence->tuple (alexandria:iota 10000000)))
;; (progn (setf 10m-seq (reduce 'fset:with-last (alexandria:iota 10000000) :initial-value (fset:empty-seq))) nil)

;; (time (dotimes (n 10000000) (insert 10m-tuple n :foo)))
;; (time (dotimes (n 10000000) (lookup 10m-tuple n)))

;; (time (dotimes (n 10000000) (fset:insert 10m-seq n :foo)))
;; (time (dotimes (n 10000000) (fset:lookup 10m-seq n)))


;; (defclass lazy-seq (standard-object sequence) ())

;; (defmethod sb-sequence:length ((sequence lazy-seq)) nil)
;; (defmethod sb-sequence:elt ((sequence lazy-seq) index) nil)
;; (defmethod (setf sb-sequence:elt) (new-value (sequence lazy-seq) index) nil)
;; (defmethod sb-sequence:adjust-sequence
;;     ((sequence lazy-seq) length &key initial-element (initial-contents nil icp))
;;   nil)
;; (defmethod sb-sequence:make-sequence-like
;;     ((sequence lazy-seq) length &key initial-element initial-contents)
;;   nil)


;; (defmethod sb-sequence:length ((sequence tuple)) (tuple-count sequence))
;; (defmethod sb-sequence:elt ((sequence tuple) index) (lookup sequence index))
;; (defmethod (setf sb-sequence:elt) (new-value (sequence tuple) index)
;;   (let ((new (insert sequence index new-value)))
;;     (with-slots (root) sequence
;;       (setf root (tuple-root new)))
;;     new-value))
;; (defmethod sb-sequence:make-sequence-like
;;     ((sequence tuple) length &key (initial-element nil iep) (initial-contents nil icp))
;;   (cond (iep (loop repeat length
;;                    for tuple = (conj (tuple) initial-element) then (conj tuple initial-element)
;;                    finally (return tuple)))
;;         (icp (loop repeat length
;;                    for element in initial-contents
;;                    for tuple = (conj (tuple) element) then (conj tuple element)
;;                    finally (return tuple)))
;;         (t (loop for n below length
;;                  for element = (ignore-errors (lookup sequence n))
;;                  for tuple = (conj (tuple) element) then (conj tuple element)
;;                  finally (return tuple)))))
;; (defmethod sb-sequence:adjust-sequence
;;     ((sequence tuple) length &key (initial-element nil iep) (initial-contents nil icp))
;;   (apply 'sb-sequence:make-sequence-like
;;          `(,sequence
;;            ,length
;;            ,@(if iep `(:initial-element ,initial-element))
;;            ,@(if icp `(:initial-contents ,initial-contents)))))

;; (concatenate 'tuple "abc" (list 1 2 3) (vector 4 5 6) (tuple 7 8 9))

;; (sort (tuple 7 8 2 7 4 6 1 0 9 19) '<)

;; (remove-duplicates (sort (tuple 7 8 2 7 4 6 1 0 9 19) '<))

;; (remove-if-not 'oddp (tuple 1 2 3 4 5 6 7 8 9 10))

;; (map 'tuple 'sqrt (vector 1 2 3))

;; (merge 'tuple (vector 1 3 5) (tuple 2 4 6) '<)2
