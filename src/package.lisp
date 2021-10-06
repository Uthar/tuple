(defpackage :tuple
  (:use :cl)
  (:export

   ;; Both a type and a tuple creation function
   :tuple

   ;; Functions for operating on tuples
   :empty-tuple
   :tuple-lookup
   :tuple-insert
   :tuple-conj
   :tuple-remove
   :tuple-size
   :tuple-reduce
   :tuple-map
   :tuple-filter
   :tuple-eq

   ;; Utilities
   :tuple->list
   :sequence->tuple))
