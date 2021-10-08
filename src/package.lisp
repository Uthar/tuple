(defpackage :tuple
  (:use :cl)
  (:shadow :pop :count :equal)
  (:export

   ;; Both a type and a tuple creation function
   ;; FIXME now theres tuple and slice and this is a leak
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

   ;; :tuple-reduce
   ;; :tuple-map
   ;; :tuple-filter
   ;; :tuple-eq

   ;; Utilities
   :tuple->list
   :sequence->tuple))
