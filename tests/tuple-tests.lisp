(ql:quickload :fiveam)

(in-package :cl-user)

(use-package :5am)

(test empty-tuple
  (let* ((tuple (empty-tuple)))
    (is (eq (tuple-count tuple) 0))))

(test 1-tuple
  (let* ((tuple (conj (empty-tuple) "foo")))
    (is (eq (tuple-count tuple) 1))))

(test 100k-tuple
  (let* ((tuple (reduce 'conj `(,(empty-tuple) ,@(alexandria:iota 100000)))))
    (is (eq (tuple-count tuple) 100000))
    (is (loop for n below 100000 always (eq (lookup tuple n) n)))))

(declaim (optimize debug))

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

(defvar 10m-tuple (reduce 'conj `(,(empty-tuple) ,@(alexandria:iota 10000000))))

(time (dotimes (n 10000000) (insert 10m-tuple n :foo)))
(time (dotimes (n 10000000) (lookup 10m-tuple n)))

(ql:quickload :fset)

(progn (defvar 10m-seq (reduce 'fset:with-last `(,(fset:empty-seq) ,@(alexandria:iota 10000000)))) nil)

(progn (time (dotimes (n 10000000) (fset:with 10m-seq n :foo))) nil)

(time (dotimes (n 10000000) (fset:lookup 10m-seq n)))
