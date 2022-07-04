;; To run clojure restricted to 1 core:
;; taskset -c 0 rlwrap clojure
;; (load-file "bench.clj")

(defn range* [n]
  (take n (repeatedly rand)))

(def arange (range* 1e6))

;; 1kk conj
(time
 (count (reduce conj arange (vector))))

(def v (into (vector) (reduce conj arange (vector))))


;; 2kk random insert
(time
 (dotimes [_ 2]
 (dotimes [n 1e6] (assoc v n n))))


;; 1kkk lookups
(time
 (dotimes [_ 10]
 (dotimes [n 1e6] (get v n))))
