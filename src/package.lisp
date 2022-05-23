(defpackage :tuple
  (:use :cl)
  (:shadow :pop :count :equal :reduce :map :remove :append)
  (:export

   ;; Both a type and a creation function
   :tuple

   ;; Functions for operating on tuples
   :lookup
   :insert
   :append
   :count
   :pop
   :slice
   :peek
   :equal
   :concat
   :remove

   ;; higher-order functions
   :map
   :reduce
   :filter

   ;; Utilities
   :tuple->list
   :sequence->tuple))
