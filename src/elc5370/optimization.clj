(ns elc5370.optimization
  (:use clojure.core)
  (:require [clojure.core.matrix :as mat])
  (:import [mikera.vectorz Vector]
           [mikera.matrixx Matrix]))

(defn newton
  [f df & {:keys [x0 iter tol] :or {x0   (double 0.0)
                                    iter (long   100)
                                    tol  (double 0.00001)}}]
  (loop [i ^Long   (long 0)
         x ^Double (double x0)]
    (if (< i (long iter))
      (let [fx (f x)]
        (if (> fx tol)
          (recur (unchecked-inc i) (+ x (/ fx (df x))))
          x))
      x)))


(defn df
  [a]
  )
