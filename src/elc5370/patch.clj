(ns elc5370.patch
  (:use [incanter.core :only ($ to-list $data)])
  (:import (org.jfree.chart ChartFactory
                            JFreeChart)
           (org.jfree.data.xy DefaultHighLowDataset
                              XYSeries
                              XYSeriesCollection
                              AbstractXYDataset)
           (org.jfree.data UnknownKeyException)))

;; This is from incanter charts, due to dependency, not part of the patch
(defn- data-as-list
  "
  data-as-list [x data]

  If x is a collection, return it
  If x is a single value, and data is undefined, return x in vector
  If x is a single value, and data is defined, return ($ x data)
  "
  [x data]
  (if (coll? x)
    (to-list x)
    (if data
      (let [selected ($ x data)]
        (if (coll? selected)
          selected
          [selected]))
      [x])))
;; ========================================================================

(defmacro get-series-idx
  [dataset label]
  `(let [c# (.getSeriesCount ~dataset)]
     (loop [idx# ^Integer (int 0)]
       (if (< idx# c#)
         (if (= ~label (.getSeriesKey ~dataset idx#))
           idx#
           (recur (unchecked-inc-int idx#)))
         nil))))

(defn get-series-by-label
  "Return the series with name specified in series-label.
   When supplied with a dataset index, this will only return the series
   with given name in that particular dataset, this will make the search
   faster."
  ([chart series-label]
   (let [plot (-> chart .getPlot)]
     (loop [n (dec (.getDatasetCount plot))]
       (let [dataset    (.getDataset plot n)
             series-idx (get-series-idx dataset series-label)
             ;; series (try
             ;;          (-> plot (.getDataset n) (.getSeries series-label))
             ;;          (catch UnknownKeyException e
             ;;            nil))
             ]
         (if series-idx
           (.getSeries dataset series-idx)
           (if (= n 0)
             nil
             (recur (dec n))))))))
  ([chart series-label index]
   (let [dataset (-> chart .getPlot (.getDataset index))]
     (when dataset
       (.getSeries dataset 0)
       ;; (try (.getSeries dataset series-label)
       ;;      (catch UnknownKeyException e
       ;;        nil))
       ))))

(defn get-render-by-label
  ([chart series-label]
   (let [plot (-> chart .getPlot)]
     (loop [n (dec (.getDatasetCount plot))]
       (let [dataset    (.getDataset plot n)
             series-idx (get-series-idx dataset series-label)
             ;; series (try
             ;;          (-> plot (.getDataset n) (.getSeries series-label))
             ;;          (catch UnknownKeyException e
             ;;            nil))
             ]
         (if series-idx
           (.getRenderer plot n)
           (if (= n 0)
             nil
             (recur (dec n)))))))))

(defn update-series
  "Identify a series by series-label, then add new data to that series.
   Optionally, old data can be removed.
   If the series cannot be found, function returns nil, and nothing will
   be changed."
  [chart series-label x y & options]
  (let [opts       (when options (apply assoc {} options))
        data       (or (:data  opts) $data)
        index      (or (:index opts) -1)
        _x         (data-as-list x data)
        _y         (data-as-list y data)
        clean?     (or (:clean opts) false)
        series     (if (>= index 0)
                     (get-series-by-label chart series-label index)
                     (:series (get-series-by-label chart series-label)))]
    (if series
      (do
        (when clean? (.clear series))
        (dorun
         (map (fn [x y]
                (if (and (not (nil? x))
                         (not (nil? y)))
                  (.add series (double x) (double y))))
              _x _y))
        chart)
      nil)))

(defn has-series?
  "Examine if series-label is in any of the datasets of given chart."
  [chart series-label]
  (if (get-series-by-label chart series-label) true false))

(defn list-series
  "List all the series label and the index of the dataset they are in."
  [chart]
  (let [plot       (-> chart .getPlot)
        count      (.getDatasetCount plot)
        series-map (transient {})]
    (doseq [n (range count)]
      (let [dataset      (.getDataset plot n)
            series-count (.getSeriesCount dataset)]
        ;; (println series-count)
        (assoc! series-map n (mapv (fn [i] (.getSeriesKey dataset i)) (range series-count)))))
    (persistent! series-map)))

(defn remove-series
  "Remove an existing series speicified by series-label.
   If the series does not exist it return nil.
   Note: this will remove duplicated series labels"
  ([chart series-label]
   (let [plot (-> chart .getPlot)]
     (loop [n     (dec (.getDatasetCount plot))
            count 0]
       (let [dataset (.getDataset plot n)
             series-idx (let [c (.getSeriesCount dataset)]
                          (loop [idx ^Integer (int 0)]
                            (if (< idx c)
                              (if (= series-label (.getSeriesKey dataset idx))
                                idx
                                (recur (unchecked-inc-int idx)))
                              nil)))
             ;; series  (try
             ;;           (.getSeries dataset series-label)
             ;;           (catch UnknownKeyException e
             ;;             nil))
             ]
         (if series-idx
           (do
             (.removeSeries dataset series-idx)
             (if (= n 0)
               (inc count)
               (recur (dec n) (inc count))))
           (if (= n 0)
             count
             (recur (dec n) count)))))))
  ([chart series-label index]
   ))

(defn clear-chart
  "Remove all series from all dataset."
  [chart]
  (let [plot       (-> chart .getPlot)
        count      (.getDatasetCount plot)]
    (doseq [n (range count)]
      (let [dataset (.getDataset plot n)
            series-count (.getSeriesCount dataset)]
        ;; (println series-count)
        (doseq [i (range series-count)]
          (.removeSeries dataset i))))
    chart))
