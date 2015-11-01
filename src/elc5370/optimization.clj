(ns elc5370.optimization
  (:use clojure.core elc5370.binomial)
  (:require [clojure.core.matrix :as mat])
  (:import [mikera.vectorz Vector]
           [mikera.matrixx Matrix]
           [org.apache.commons.math3.special Beta]))

(def beta-binomial (new-beta-binomialmial-fn 26 0.68 2.7))
(def tol 0.000001)

(defn newton
  [f df & {:keys [x0 iter tol] :or {x0   (double 0.0)
                                    iter (long   100)
                                    tol  (double 0.000001)}}]
  (loop [i ^Long   (long 0)
         x ^Double (double x0)]
    (if (< i (long iter))
      (let [fx (f x)]
        (if (> (Math/abs fx) tol)
          (recur (unchecked-inc i) (- x (/ fx (df x))))
          x))
      x)))

(defn f
  [a]
  (loop [x ^Long   (long 0)
         s ^Double (double 0.0)]
    (if (<= x 26)
      (let [_2x   ^Long   (long (* x 2))
            CxBxB ^Double (beta-binomial x)]
        (recur (unchecked-inc x)
               (+    s
                     (Math/pow a (+ _2x 2))
                  (- (* 2 (Math/pow a (inc _2x))))
                     (Math/pow a _2x)
                     (* 2 CxBxB (Math/pow a (inc x)))
                  (- (* 2 CxBxB (Math/pow a x)))
                     (Math/pow CxBxB 2))))
      s)))

(defn df
  [a]
  (loop [x ^Long   (long 1)
         s ^Double (double 0.0)]
    (if (<= x 26)
      (let [_2x     ^Long   (long (* x 2))
            _2CxBxB ^Double (double (* 2 (beta-binomial x)))]
        (recur (unchecked-inc x)
               (+    s
                     (* (+ _2x 2) (Math/pow a (inc _2x)))
                  (- (* (+ (* 4 x) 2) (Math/pow a _2x)))
                     (* _2x (Math/pow a (unchecked-dec _2x)))
                     (* (inc x) _2CxBxB (Math/pow a x))
                  (- (* x _2CxBxB (Math/pow a (unchecked-dec x)))))))
      (+ s (* 2 a) (- 2) (* 2 (beta-binomial 0))))))

(defn ddf
  [a]
  (loop [x ^Long   (long 2)
         s ^Double (double 0.0)]
    (if (<= x 26)
      (let [_2x     ^Long   (long (* x 2))
            _2CxBxB ^Double (double (* 2 (beta-binomial x)))]
        (recur (unchecked-inc x)
               (+    s
                     (* (+ _2x 2) (+ _2x 1) (Math/pow a _2x))
                  (- (* (+ _2x 2) _2x       (Math/pow a (dec _2x))))
                     (* _2x (dec _2x)       (Math/pow a (- _2x 2)))
                     (* x (inc x) _2CxBxB (Math/pow a (dec x)))
                  (- (* x (dec x) _2CxBxB (Math/pow a (- x 2)))))))
      (+ s
         (* 12 (Math/pow a 2))
         (- (* 12 a))
         4
         (* 4 (beta-binomial 1))))))

(newton df ddf :x0 1 :iter 10000 :tol tol)
