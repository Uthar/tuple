(require :asdf)
(pushnew (truename "../") asdf:*central-registry*)
(asdf:load-system :tuple)

(setf tup (tuple:tuple 1 2 "foo" 3.14))

(tuple:conj tup "bar")

(tuple:pop tup)

(tuple:insert tup 0 "baz")

(tuple:lookup tup 3)

(tuple:peek tup)

(tuple:equal tup tup)

(tuple:slice tup 1 3)

(tuple:count tup)

(tuple:concat tup (tuple:tuple 42 43 44))

;; Optional `[` reader macro
(asdf:load-system :tuple/reader)
(tuple:equal [1 2 (+ 1 2)] (tuple:tuple 1 2 3))
