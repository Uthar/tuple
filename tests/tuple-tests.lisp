(in-package :tuple/test)

(in-suite :tuple)

(defun range (n)
  (loop for x below n collect x))

(test empty-tuple
  (let* ((tuple (empty-tuple)))
    (is (eq (tuple-size tuple) 0))))

(test 1-tuple
  (let* ((tuple (tuple "foo")))
    (is (eq (tuple-size tuple) 1))
    (is (equal (tuple-lookup tuple 0) "foo"))))

(test 100k-tuple
  (let* ((tuple (sequence->tuple (range 100000))))
    (is (eq (tuple-size tuple) 100000))
    (is (loop for n below 100000 always (eq (tuple-lookup tuple n) n)))))

(test immutable
  (let* ((tup1 (tuple "foo" "bar"))
         (tup2 (tuple-conj tup1 "baz")))
    (tuple-insert tup2 0 :x)
    (is (equal (tuple-lookup tup1 0) "foo"))
    (is (equal (tuple-lookup tup2 0) "foo"))
    (tuple-insert tup2 1 :y)
    (is (equal (tuple-lookup tup1 1) "bar"))
    (is (equal (tuple-lookup tup2 1) "bar"))
    (is (equal (tuple-lookup tup2 2) "baz"))))

(test tail-copied
  (let* ((tup1 (sequence->tuple (range 32)))
         (tup2 (tuple-conj tup1 :foo)))
    (is (= (tuple-size tup1) 32))
    (is (= (tuple-size tup2) 33))
    (is (= (length (tuple::tuple-tail tup1)) 32))
    (is (= (length (tuple::tuple-tail tup2)) 1))
    (is (equalp (tuple::tuple-tail tup1)
                (tuple::node-array (aref (tuple::node-array (tuple::tuple-root tup2)) 0))))
    (is (loop with tail = (tuple::tuple-tail tup1)
              for n below 32
              always (= (aref tail n) n)))
    (is (eq (aref (tuple::tuple-tail tup2) 0) :foo))))

(test share-root
  (let* ((tup1 (sequence->tuple (range 1056)))
         (tup2 (tuple-conj tup1 :foo)))
    (is (= (tuple-size tup1) 1056))
    (is (= (tuple-size tup2) 1057))
    (is (= (length (tuple::tuple-tail tup1)) 32))
    (is (= (length (tuple::tuple-tail tup2)) 1))
    (is (eq (tuple::tuple-root tup1) (aref (tuple::node-array (tuple::tuple-root tup2)) 0)))
    (is (eq (aref (tuple::tuple-tail tup2) 0) :foo))))


(test 10k-insert
  (let* ((tuple (sequence->tuple (range 10000)))
         (vals (make-array 10000
                           :initial-contents
                           (loop for n below 10000 collect (funcall (gen-string))))))
    (is (eq (tuple-size tuple) 10000))
    (is (loop for n below 10000
              for tup = (tuple-insert tuple n (aref vals n)) then (tuple-insert tup n (aref vals n))
              always (loop for x below n always (equal (tuple-lookup tup x) (aref vals x)))
              always (loop for x from (1+ n) below 10000 always (equal (tuple-lookup tup x) x))))))
