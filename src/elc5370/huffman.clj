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

(defn make-dummies
  [n]
  (for [i (range n)]
    [(str/join ["_dummy" (.toString i) "_"]) (int 0)]))

(defn padding
  "Calculate number of merges, and number of padding to sorted symbol table"
  [n D]
  (let [k (long (Math/ceil (/ (dec n) (dec D))))]
    {:merges k :pad (- (inc (* k (dec D))) n)}))

(defn make-tree
  [lst & {:keys [D] :or {D 2}}]
  (let [{:keys [merges pad]} (padding (count lst) D)]
    (loop [s (concat (make-dummies pad) lst)
           k merges]
      (if (> k 0)
        (let [[h _s] (split-at D s)
              v      (apply + (map second h))]
          (recur (concat (filter #(<  (second %) v) _s)
                         `(~[h v])
                         (filter #(>= (second %) v) _s))
                 (dec k)))
        (first s)))))

(defn gen-code
  [tree & {:keys [code] :or {code ["0" "1"]}}]
  (let [D-1 (dec (count code))]
    (letfn [(walk [t s m!]
              ;; (println t s)
              (let [branches (first t)]
                (if (coll? branches)
                  (let [m (loop [n  0
                                 _m m!]
                            (if (< n D-1)
                              (recur (inc 0) (walk (nth branches n) (str/join [s (nth code n)]) _m))
                              _m))]
                    (recur (last branches) (str/join [s (last code)]) m))
                  (assoc! m! branches s))))]
      (persistent! (walk tree "" (transient {}))))))

(defn huffman-code
  [s & {:keys [code] :or {code ["0" "1"]}}]
  (let [D (count code)]
    (-> (elem-count s)
        (make-tree :D D)
        (gen-code :code code))))

;; Test
(huffman-code "DAVID CAMERON SETS OUT HIS FOUR EU REFORM GOALS  BUT THE EUROPEAN COMMISSION WARNS HIS BENEFIT RESTRICTIONS MAY BE ILLEGAL.")
