# tuple

This library implements a persistent, immutable, sequential collection type in Common Lisp

Warning: This library is __EXPERIMENTAL__ and everything can change

## goals

Primary goals of the project are:

1. Learn about and understand the persistent vector data structure,
first introduced in Clojure, by implementing it from scratch

2. Have some fun

Eventually, this could produce a library usable by real programs, but
it's a secondary goal. I don't know how idiomatic that would be, or if
it would be faster than existing, similiar libraries.

## loading

The core library has no dependencies. Tests require `fiveam`, and benchmarks require `trivial-garbage`.

To load, you can use ASDF:

```
(push (truename "path/to/git/repo/") asdf:*central-registry*)
(asdf:load-system :tuple)
```

Alternatively, simply load `src/package.lisp` and `src/tuple.lisp`:
```
(load "src/package.lisp")
(load "src/tuple.lisp")
```


## basic use

One can start using this library by evaluating expressions as below. The API is documented in `tuple.lisp`.

- Create:
```
(setf tup (tuple:tuple 1 2 "foo" 3.14))
```

- Conj
```
(tuple:conj tup "bar")
```

- Pop
```
(tuple:pop tup)
```

- Insert
```
(tuple:insert tup 0 "baz")
```

- Lookup
```
(tuple:lookup tup 3)
```

- Peek
```
(tuple:peek tup)
```

- Equal
```
(tuple:equal tup tup)
```

- Slice
```
(tuple:slice tup 1 3)
```

- Count
```
(tuple:count tup)
```

- Optional `[` reader macro in `reader.lisp`:
```
(load "src/reader.lisp") ;; or (asdf:load-system :tuple/reader)
(tuple:equal [1 2 (+ 1 2)] (tuple:tuple 1 2 3))
```

## CL implementations

Known working:
 - SBCL
 - ABCL
 - ECL
 - CCL
 - Clasp

## performance

According to the completely unscientific benchmarks in `bench.lisp`,
operations are 1.5-3x slower than Clojure on SBCL (single core, tested
on SBCL 2.1.9, x64 Linux). I.e. there's room for improvement :)

Other implementations don't have compilers as good as SBCL, so
performance will suffer (On ABCL you could just wrap Clojure's Java
implementation, though).

## resources

- talk by Mohit Thatte https://www.youtube.com/watch?v=7BFF50BHPPo
- Polymatheia blog https://hypirion.com/musings/understanding-persistent-vector-pt-1
- Clojure reference https://clojure.org/reference/data_structures#Vectors

## other immutable data structure projects

- [FSet](https://github.com/slburson/fset)
- [cloture](https://github.com/ruricolist/cloture)
- [cl-hamt](https://github.com/danshapero/cl-hamt)

## license

FreeBSD - see COPYING
