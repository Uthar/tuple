(defpackage :cl-tuple
  (:nicknames :tuple)
  (:use :cl)
  (:export
   :tuple
   ;; Functions for operating on tuples
   :empty-tuple
   :tuple-lookup
   :tuple-insert
   :tuple-remove
   :tuple-size
   :tuple-reduce
   :tuple-map
   :tuple-filter
   :tuple-eq
   ;; Utilities
   :tuple->list
   :sequence->tuple))
