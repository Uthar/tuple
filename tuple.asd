(defsystem :tuple
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
