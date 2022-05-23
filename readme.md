# tuple

This library implements a persistent immutable vector in Common Lisp.

Warning: This library is __EXPERIMENTAL__ and everything can change

## goals

Primary goals of the project are:

1. Understand the persistent vector introduced in Clojure by implementing it from scratch

2. Have some fun

## loading

Load:

```
(asdf:load-system :tuple)
```

Test:

```
(asdf:test-system :tuple)
```

Benchmark:

```
(asdf:load-system :tuple/bench)
```

## basic use

Take a look in `examples/`. The API is documented in `tuple.lisp`.

## CL implementations

Known working:
 - SBCL
 - ABCL
 - ECL
 - CCL
 - Clasp

## performance

According to the completely unscientific benchmarks in `bench.lisp`:

- OS: X86-64 Linux
- CPU: i5-3340M

### SBCL 2.2.0

- append
```
Evaluation took:
  0.237 seconds of real time
  0.237132 seconds of total run time (0.202138 user, 0.034994 system)
  [ Run times consist of 0.075 seconds GC time, and 0.163 seconds non-GC time. ]
  100.00% CPU
  639,851,499 processor cycles
  380,199,296 bytes consed
```

- insert
```
Evaluation took:
  0.650 seconds of real time
  0.649155 seconds of total run time (0.604105 user, 0.045050 system)
  [ Run times consist of 0.030 seconds GC time, and 0.620 seconds non-GC time. ]
  99.85% CPU
  1,751,608,347 processor cycles
  2,399,947,664 bytes consed
```

- lookup
```
Evaluation took:
  0.228 seconds of real time
  0.227378 seconds of total run time (0.227378 user, 0.000000 system)
  99.56% CPU
  613,114,533 processor cycles
  0 bytes consed
```

### Clojure 1.10.3 (OpenJDK 17)

- append
```
"Elapsed time: 211.689438 msecs"
```

- insert
```
"Elapsed time: 341.753422 msecs"
```

- lookup
```
"Elapsed time: 213.015009 msecs"
```

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

GPLv3
