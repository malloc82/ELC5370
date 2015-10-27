(ns elc5370.core
  (:use clojure.core
        elc5370.patch)
  (:require [clojure.java.io :as io]
            [incanter.core   :as incanter]
            [incanter.charts :as charts]
            [incanter.stats  :as stats]
            [clojure.core.matrix :as mat]
            )
  (:import [mikera.matrixx Matrix]
           [mikera.matrixx.impl StridedMatrix]
           [mikera.vectorz Vector]
           [java.awt Color]))

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
            {:letters (mapv first v)
             :index   (range (count v))
             :Pr      (mapv #(/ % s) p)}))

(def ch (charts/scatter-plot (data :index) (data :Pr)
                             :x-label "Letters"
                             :y-label "Probability"
                             :series-label "Raw data"
                             :legend true))

(defn plot-poisson
  [shift lambda]
  (let [poisson-data (stats/pdf-poisson (vec (range shift (+ (count (:Pr data)) shift))) :lambda lambda)
        label        "Poisson Fit"]
    (remove-series ch label)
    (charts/add-lines ch (vec (range (count (:Pr data)))) poisson-data :series-label label)))

(defn plot-exp
  [shift mean & {:keys [color]}]
  (let [exp-data (stats/pdf-exp (vec (range shift (+ (count (:Pr data)) shift))) :rate (/ 1.0 (double mean)))
        label    "Exp Fit"]
    (remove-series ch label)
    (charts/add-lines ch (vec (range (count (:Pr data)))) exp-data :series-label label)
    (when color
      (let [renderer (get-render-by-label ch label)]
        (when-not (nil? renderer)
          (.setSeriesPaint (:renderer renderer) 0 color))))))

