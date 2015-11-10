(ns elc5370.huffman
  (:use clojure.core)
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn elem-count
  [s]
  (let [total (count s)]
    (loop [s s
           m (transient {})
           len total]
      (if (> len 0)
        (let [k (first s)]
          (recur (rest s) (assoc! m k (inc (or (m k) 0))) (dec len)))
        (map (fn [[k v]]
               [k (/ v total)])
             (sort-by second < (persistent! m)))))))


(defn make-tree
  [lst]
  (loop [s   lst
         len (count lst)]
    (if (> len 1)
      (let [one (first s)
            two (second s)
            v   (+ (second one) (second two))
            t   (list [[one two] v])
            sub-lst (drop 2 s)]
        ;; (println s)
        ;; (println one)
        ;; (println two)
        ;; (println v)
        ;; (println t)
        ;; (println sub-lst)
        ;; (println "=====================================")
        (recur (concat (filter #(< (second %) v) sub-lst)
                       t
                       (filter #(>= (second %) v) sub-lst))
               (dec len)))
      (first s))))

(defn gen-code
  [tree]
  (letfn [(f [t s m!]
            ;; (println t s)
            (if (coll? (first t))
              (let [_m (f (first  (first t)) (str/join [s "0"]) m!)]
                (f (second (first t)) (str/join [s "1"]) _m))
              (do
                (assoc! m! (first t) s))))]
    (persistent! (f tree "" (transient {})))))


;; Test
(gen-code (make-tree (elem-count "DAVID CAMERON SETS OUT HIS FOUR EU REFORM GOALS  BUT THE EUROPEAN COMMISSION WARNS HIS BENEFIT RESTRICTIONS MAY BE ILLEGAL.")))
