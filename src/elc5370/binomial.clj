(ns elc5370.binomial
  (:use clojure.core)
  (:import [org.apache.commons.math3.special Beta]))

(def comb-cache
  (let [cache (atom {})]
    (fn ! ([^Long n ^Long k]
          (or (@cache [n k])
              (cond
                (or (= n k) (= k 0)) 1
                (or (= k 1) (= k (dec n))) n
                :else (let [v ^Long (+ (! (unchecked-dec n) (unchecked-dec k))
                                       (! (unchecked-dec n) k))]
                        (swap! cache assoc [n k] v)
                        v))))
      ([] @cache))))

(def comb-mem
  (memoize
   (fn [n k]
     (cond
       (or (= n k) (= k 0)) 1
       (or (= k 1) (= k (dec n))) n
       (= [n k] [1 0]) 1
       (= [n k] [1 1]) 1
       :else (+ (comb-mem (- n 1) (- k 1))
                (comb-mem (- n 1) k))))))

(defn new-beta-fn
  [n a b]
  (memoize (fn [x]
             (if (< n x)
               (throw (Exception. (format "Beta: Combination of range, x > n: x = %d, n = %d" x n)))
               (Math/exp (Beta/logBeta (+ x a) (+ (- n x) b)))))))

(defn beta-binomial
  [k & {:keys [n alpha beta color] :or {n 10 alpha 0.7 beta 2}}]
  (let [Beta-fn (new-beta-fn n alpha beta)
        B       (Math/exp (Beta/logBeta alpha beta))
        Bb-fn   (fn [x]
                  (if (< n x)
                    (throw (Exception. (format "Beta-binomial: Combination of range, x > n: x = %d, n = %d" x n)))
                    (* (comb-cache n x)
                       (/ (Beta-fn x) B))))]
    (if (coll? k)
      (map Bb-fn k)
      (Bb-fn k))))

