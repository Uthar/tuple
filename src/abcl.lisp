(in-package :tuple)

(declare (optimize speed))
(declaim (inline lookup insert conj count pop slice peek tuple))

(defun tuple (&rest elems)
  (if (zerop (length elems))
      (java:jfield  "clojure.lang.PersistentVector" "EMPTY")
      (java:jstatic "adopt" "clojure.lang.PersistentVector"
                    (java:jarray-from-list elems))))

(defvar |Object| (java:jclass "java.lang.Object"))
(defvar |PersistentVector| (java:jclass "clojure.lang.PersistentVector"))
(defvar |get| (java:jmethod |PersistentVector| "get" "int"))
(defvar |assoc| (java:jmethod |PersistentVector| "assoc" |Object| |Object|))
(defvar |cons| (java:jmethod |PersistentVector| "cons" "java.lang.Object"))
(defvar |count| (java:jmethod |PersistentVector| "count"))
(defvar |pop| (java:jmethod |PersistentVector| "pop"))
(defvar |subList| (java:jmethod |PersistentVector| "subList" "int" "int"))
(defvar |peek| (java:jmethod |PersistentVector| "peek"))

(defun lookup (tuple index)
  (java:jcall |get| tuple index))

(defun insert (tuple index val)
  (java:jcall |assoc| tuple index val))

(defun conj (tuple val)
  (java:jcall |cons| tuple val))

(defun count (tuple)
  (java:jcall |count| tuple))

(defun pop (tuple)
  (java:jcall |pop| tuple))

(defun slice (tuple start &optional end)
  (java:jcall |subList| tuple start (or end (count tuple))))

(defun peek (tuple)
  (java:jcall |peek| tuple))
