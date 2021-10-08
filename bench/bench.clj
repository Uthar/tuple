;; To run clojure restricted to 1 core:
;; taskset -c 0 rlwrap clojure

(defn range* [n]
  (take n (repeatedly rand)))

;; 1kkk conj
(time
 (do (reduce conj `(~(vector) ~@(range* 1e7))) nil))

(def v (apply vector (range 1e6)))


;; 1kk random insert
(time
 (dotimes [n 1e6] (assoc v n (rand 100))))


;; 1kkkk lookups
(time
 (dotimes [n 1e8] (get v n)))
