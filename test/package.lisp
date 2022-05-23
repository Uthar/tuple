(defpackage :tuple/test
  (:use :cl :5am :tuple)
  (:shadowing-import-from :cl :equal :reduce :map)
  (:shadowing-import-from :tuple :pop :count :append :remove))

(in-package :tuple/test)

(def-suite :tuple)
