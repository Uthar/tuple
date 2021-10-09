(defsystem :tuple
  :description "Immutable, persistent tuple (vector) data structure"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "tuple"))
  :in-order-to ((test-op (test-op :tuple/test))))

(defsystem :tuple/test
  :depends-on ("tuple" "fiveam")
  :pathname "test"
  :serial t
  :components ((:file "package")
               (:file "tuple-test"))
  :perform (test-op (o c) (symbol-call :5am :run! :tuple)))

(defsystem :tuple/bench
  :depends-on ("tuple" "trivial-garbage")
  :pathname "bench"
  :serial t
  :components ((:file "bench")))

(defsystem :tuple/reader
  :depends-on ("tuple")
  :pathname "src"
  :components ((:file "reader")))
