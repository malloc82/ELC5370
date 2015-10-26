(ns elc5370.core
  (:use clojure.core
        clj-plot.patch)
  (:require [clojure.java.io :as io]
            [incanter.core   :as incanter]
            [incanter.charts :as charts]
            [incanter.stats  :as stats]
            [clojure.core.matrix :as mat]
            )
  (:import [mikera.matrixx Matrix]
           [mikera.matrixx.impl StridedMatrix]
           [mikera.vectorz Vector]))

(set! *warn-on-reflection* true)

