(defpackage :tuple
  (:use :cl)
  (:shadow :pop :count :equal :reduce :map)
  (:export

   ;; Both a type and a creation function
   :tuple

   ;; Functions for operating on tuples
   :lookup
   :insert
   :conj
   :count
   :pop
   :slice
   :peek
   :equal

   ;; higher-order functions
   :map
   :reduce
   :filter

   ;; Utilities
   :tuple->list
   :sequence->tuple))
