(in-package :tuple/test)

(in-suite :tuple)

(defun range (n)
  (loop for x below n collect x))

(test empty-tuple
  (let* ((tuple (tuple)))
    (is (= (count tuple) 0))))

(test 1-tuple
  (let* ((tuple (tuple "foo")))
    (is (= (count tuple) 1))
    (is (equal (lookup tuple 0) "foo"))))

(test 100k-tuple
  (let* ((tuple (sequence->tuple (range 100000))))
    (is (= (count tuple) 100000))
    (is (loop for n below 100000 always (= (lookup tuple n) n)))))

(test immutable
  (let* ((tup1 (tuple "foo" "bar"))
         (tup2 (conj tup1 "baz")))
    (insert tup2 0 :x)
    (is (equal (lookup tup1 0) "foo"))
    (is (equal (lookup tup2 0) "foo"))
    (insert tup2 1 :y)
    (is (equal (lookup tup1 1) "bar"))
    (is (equal (lookup tup2 1) "bar"))
    (is (equal (lookup tup2 2) "baz"))))

(test tail-copied
  (let* ((tup1 (sequence->tuple (range 32)))
         (tup2 (conj tup1 :foo)))
    (is (= (count tup1) 32))
    (is (= (count tup2) 33))
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
         (tup2 (conj tup1 :foo)))
    (is (= (count tup1) 1056))
    (is (= (count tup2) 1057))
    (is (= (length (tuple::tuple-tail tup1)) 32))
    (is (= (length (tuple::tuple-tail tup2)) 1))
    (is (eq (tuple::tuple-root tup1) (aref (tuple::node-array (tuple::tuple-root tup2)) 0)))
    (is (eq (aref (tuple::tuple-tail tup2) 0) :foo))))


(test 1k-insert
  (let* ((tuple (sequence->tuple (range 1057)))
         (vals (make-array 1057
                           :initial-contents
                           (loop for n below 1057 collect (funcall (gen-string))))))
    (is (= (count tuple) 1057))
    (is (loop for n below 1057
              for tup = (insert tuple n (aref vals n)) then (insert tup n (aref vals n))
              always (loop for x below n always (equal (lookup tup x) (aref vals x)))
              always (loop for x from (1+ n) below 1057 always (= (lookup tup x) x))))))

(test pop-test
  (is (= 3 (count (pop (tuple 1 2 3 4)))))
  (is (= 1056 (count (pop (sequence->tuple (range 1057))))))
  (is (= 32 (count (pop (sequence->tuple (range 33))))))

  (is (eq :foo (peek (conj (pop (pop (tuple 1 2 3 4))) :foo))))
  (is (eq :foo (peek (conj (pop (pop (sequence->tuple (range 1057)))) :foo))))
  (is (eq :foo (peek (conj (pop (pop (sequence->tuple (range 33)))) :foo)))))


(test slice-test
  (is (= 3 (count (slice (tuple 1 2 3 4) 1))))
  (is (= 1056 (count (slice (sequence->tuple (range 1057)) 1))))
  (is (= 32 (count (slice (sequence->tuple (range 33)) 1))))

  (is (= 2 (count (slice (slice (tuple 1 2 3 4) 1) 1))))
  (is (= 1055 (count (slice (slice (sequence->tuple (range 1057)) 1) 1))))
  (is (= 31 (count (slice (slice (sequence->tuple (range 33)) 1) 1)))))

(test equal-test
  (is (tuple:equal (tuple 1 2 3) (tuple 1 2 3)))
  (is (tuple:equal (slice (tuple -1 0 1 2 3 4 5) 2 5) (tuple 1 2 3)))
  (is (tuple:equal (tuple 1 2 3) (slice (tuple -1 0 1 2 3 4 5) 2 5) ))
  (is (tuple:equal (slice (tuple -1 0 1 2 3 4 5) 2 5) (slice (tuple -1 0 1 2 3 4 5) 2 5) ))
  (is (tuple:equal (slice (slice (tuple -1 0 1 2 3 4 5) 2 5) 1) (tuple 2 3)))
  (is (tuple:equal (tuple 2 3) (slice (slice (tuple -1 0 1 2 3 4 5) 2 5) 1) ))
  (is (tuple:equal (slice (slice (tuple -1 0 1 2 3 4 5) 2 5) 1)
                   (slice (slice (tuple -1 0 1 2 3 4 5) 2 5) 1)))

  (is (tuple:equal (tuple (tuple (tuple 1))) (tuple (tuple (tuple 1)))))
  (is (tuple:equal (slice (tuple :foo (tuple (tuple 1))) 1) (tuple (tuple (tuple 1))))))
