(in-package :tuple)

(defmethod tuple (&rest elems)
  (if (zerop (length elems))
      (java:jfield  "clojure.lang.PersistentVector" "EMPTY")
      (java:jstatic "adopt" "clojure.lang.PersistentVector"
                    (java:jarray-from-list elems))))

(defmethod lookup (tuple index)
  (java:jcall "nth" tuple index))

(defmethod insert (tuple index val)
  (java:jcall "assoc" tuple index val))

(defmethod conj (tuple val)
  (java:jcall "cons" tuple val))

(defmethod count (tuple)
  (java:jcall "count" tuple))

(defmethod pop (tuple)
  (java:jcall "pop" tuple))

(defmethod slice (tuple start &optional end)
  (java:jcall "subList" tuple start (or end (count tuple))))

(defmethod peek (tuple)
  (java:jcall "peek" tuple))
