(ns elc5370.core
  (:use clojure.core
        elc5370.patch)
  (:require [clojure.java.io :as io]
            [incanter.core   :as incanter]
            [incanter.charts :as charts]
            [incanter.stats  :as stats]
            [clojure.core.matrix :as mat]
            [clojure.pprint  :refer [pprint]]
            )
  (:import [mikera.matrixx Matrix]
           [mikera.matrixx.impl StridedMatrix]
           [mikera.vectorz Vector]
           [java.awt Color Shape]
           [java.awt.geom Ellipse2D$Double]
           [org.apache.commons.math3.special Beta]))

(set! *warn-on-reflection* true)

(def table {"SPC" 40000
            "E"	  21912
            "T"	  16587
            "A"	  14810
            "O"	  14003
            "I"	  13318
            "N"	  12666
            "S"	  11450
            "R"	  10977
            "H"	  10795
            "D"	  7874
            "L"	  7253
            "U"	  5246
            "C"	  4943
            "M"	  4761
            "F"	  4200
            "Y"	  3853
            "W"	  3819
            "G"	  3693
            "P"	  3316
            "B"	  2715
            "V"	  2019
            "K"	  1257
            "X"	  315
            "Q"	  205
            "J"	  188
            "Z"	  128})

(def data (let [v (sort-by second > table)
                p (mapv second v)
                s (double (apply + p))]
            (println "sum = " s)
            {:letters (mapv first v)
             :index   (range (count v))
             :Pr      (mapv #(/ % s) p)}))

(def x (apply concat
              (map (fn [x i] (repeat (second x) i))
                   (sort-by second > table)
                   (range (count table)))))

(defn comb
  [^Long n ^Long k]
  (let [num (loop [i ^Long (long (+ 1 (- n k)))
                   p ^Long (long 1)]
              (if (< i n)
                (recur (unchecked-inc i) (* p i))
                (* p i)))
        den (if (= k 0)
              1
              (loop [i ^Long (long k)
                     p ^Long (long 1)]
                (if (> i 1)
                  (recur (unchecked-dec i) (* p i))
                  p)))]
    (print num den)
    (cond
      (= k 0) 1
      :else (/ num den))))

(def comb-cache
  (let [cache (atom {})]
    (fn ! ([^Long n ^Long k]
          (let [m ^Long (@cache [n k])]
            (if m
              m
              (cond
                (or (= n k) (= k 0)) 1
                (or (= k 1) (= k (dec n))) n
                :else (let [v ^Long (+ (! (unchecked-dec n) (unchecked-dec k))
                                       (! (unchecked-dec n) k))]
                        (swap! cache assoc [n k] v)
                        v)))))
      ([] @cache))))

(def comb-mem
  (memoize (fn [n k]
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

(defn p-fn
  [x  & {:keys [alpha] :or {alpha 0.5}}]
  (let [f (fn [x]
            (* (- 1 alpha) (Math/pow alpha x)))]
    (if (coll? x)
      (map f x)
      (f x))))

(defn plot-poisson
  [lambda & {:keys [color]}]
  (let [poisson-data (stats/pdf-poisson (vec (range (count (:Pr data)))) :lambda lambda)
        label        "Poisson Fit"]
    (remove-series ch label)
    (charts/add-lines ch (vec (range (count (:Pr data)))) poisson-data :series-label label)
    (when color
      (let [renderer (get-render-by-label ch label)]
        (when-not (nil? renderer)
          (.setSeriesPaint renderer 0 color))))))

(defn plot-exp
  [mean & {:keys [color]}]
  (let [exp-data (stats/pdf-exp (vec (range (count (:Pr data)))) :rate (/ 1.0 (double mean)))
        label    "Exp Fit"]
    (remove-series ch label)
    (charts/add-lines ch (vec (range (count (:Pr data)))) exp-data :series-label label)
    (when color
      (let [renderer (get-render-by-label ch label)]
        (when-not (nil? renderer)
          (.setSeriesPaint renderer 0 color))))))

(defn plot-beta-binomial
  [alpha beta & {:keys [color]}]
  (let [b-b-data (beta-binomial (vec (range 27))
                                :n 26 :alpha alpha :beta beta)
        label    "Beta-Binomail Fit"]
    (remove-series ch label)
    (charts/add-lines ch (vec (range 27)) b-b-data :series-label label)
    (when color
      (let [renderer (get-render-by-label ch label)]
        (when-not (nil? renderer)
          (.setSeriesPaint renderer 0 color))))))

(defn KLIC-min-alpha [n a b]
  (let [beta-fn (new-beta-fn n a b)
        num     (loop [i 0
                       s 0]
                  ;; (println i)
                  (if (<= i n)
                    (recur (inc i) (+ s (* (comb-cache n i) (beta-fn i) i)))
                    s))
        den     (loop [i 0
                       s 0]
                  (if (<= i n)
                    (recur (inc i) (+ s (* (comb-cache n i) (beta-fn i) (inc i))))
                    s))]
    ;; (println num)
    ;; (println den)
    (/ num den)))

(defn plot-p-fn
  [& {:keys [color alpha] :or {alpha (KLIC-min-alpha 26 0.68 2.7)}}]
  (let [px (p-fn (vec (range 27)) :alpha alpha)
        label "p(x) fit"]
    (remove-series ch label)
    (charts/add-lines ch (vec (range 27)) px :series-label label)
    (when color
      (let [renderer (get-render-by-label ch label)]
        (when-not (nil? renderer)
          (.setSeriesPaint renderer 0 color))))))


;; (def ch (charts/scatter-plot (data :index) (data :Pr)
;;                              :x-label "Letters"
;;                              :y-label "Probability"
;;                              :series-label "Raw data"
;;                              :legend true))

(def ch (charts/histogram x :nbins (count table)
                          :x-label "Letters" :y-label "Probability"
                          :series-label "Letter Histogram"
                          :legend true :density true))
(charts/add-points ch (data :index) (data :Pr) :series-label "Raw data")
(let [renderer (get-render-by-label ch "Raw data")]
  (.setSeriesShape renderer 0 (Ellipse2D$Double. -3 -3 6 6)))

(plot-exp 6.5 :color Color/RED)
(plot-beta-binomial 0.68 2.7 :color Color/BLUE)

(let [n 26
      a 0.68
      b 2.7
      beta-fn (new-beta-fn n a b)
      B       (Math/exp (Beta/logBeta a b))
      s       (loop [i 0
                     s 0]
                ;; (println i)
                (if (<= i n)
                  (recur (inc i) (+ s (/ (* (comb-cache n i) (beta-fn i)) B)))
                  s))]
    ;; (println num)
    ;; (println den)
    s)
