;; To run clojure restricted to 1 core:
;; taskset -c 0 rlwrap clojure
;; (load-file "bench.clj")

(defn range* [n]
  (take n (repeatedly rand)))

;; 1kk conj
(time
 (do (reduce conj `(~(vector) ~@(range* 1e6))) nil))

(def v (apply vector (range 1e6)))


;; 2kk random insert
(time
 (dotimes [_ 2]
 (dotimes [n 1e6] (assoc v n (rand 100)))))


;; 1kkk lookups
(time
 (dotimes [_ 10]
 (dotimes [n 1e6] (get v n))))
