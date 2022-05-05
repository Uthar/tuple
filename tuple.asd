(defsystem :tuple
  :description "Immutable, persistent tuple (vector) data structure"
  :version "0.1.0"
  :license "FreeBSD"
  :pathname "src"
  :components ((:file "package")
               (:file "tuple")
               #+abcl (:file "abcl")
               #-abcl (:file "generic"))
  :in-order-to ((test-op (test-op :tuple/test))))

(defsystem :tuple/test
  :depends-on ("tuple" "fiveam")
  :pathname "test"
  :components ((:file "package")
               (:file "tuple-test"))
  :perform (test-op (o c) (symbol-call :5am :run! :tuple)))

(defsystem :tuple/bench
  :depends-on ("tuple" "trivial-garbage")
  :pathname "bench"
  :components ((:file "bench")))

(defsystem :tuple/reader
  :depends-on ("tuple")
  :pathname "src"
  :components ((:file "reader")))
