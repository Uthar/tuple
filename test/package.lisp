(defpackage :tuple/test
  (:use :cl :5am :tuple)
  (:shadowing-import-from :cl :equal)
  (:shadowing-import-from :tuple :pop :count))

(in-package :tuple/test)

(def-suite :tuple)
