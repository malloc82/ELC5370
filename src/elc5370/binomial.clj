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
  [n alpha beta]
  (memoize (fn [^Long x]
             (if (< n x)
               (throw (Exception. (format "Beta: Combination of range, x > n: x = %d, n = %d" x n)))
               (Math/exp (Beta/logBeta (+ x alpha) (+ (- n x) beta)))))))

(defn new-beta-binomial-fn
  [n alpha beta]
  (let [cache   (atom {})
        beta-fn (new-beta-fn n alpha beta)
        B       (Math/exp (Beta/logBeta alpha beta))]
    (fn ([^Long x]
        (let [bb-fn (fn [x]
                      (cond
                        (< x 0) (throw
                                 (Exception.
                                  (format "Beta-binomial: Index must be non negative integer: x = %d" x)))
                        (< n x) (throw
                                 (Exception.
                                  (format "Beta-binomial: Combination values of range: n = %d, x = %d" n x)))
                        :else (or (@cache x)
                                  (let [val (* (comb-cache n x)
                                               (/ (beta-fn x) B))]
                                    (swap! cache assoc x val)
                                    val))))]
          (if (coll? x) (map bb-fn x) (bb-fn x))))
      ([] @cache))))

