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
  "Calculate number of merges, and number of padding to sorted symbol table.
   Total, there should be 1 + k(D - 1) symbols, so we need to calculate
   the number of dummies to pad."
  [n D]
  (let [k (long (Math/ceil (/ (dec n) (dec D))))]
    {:merges k :pad (- (inc (* k (dec D))) n)}))

(defn make-tree
  "Create a huffman tree given a sorted alphabet list based on frequency."
  [lst & {:keys [D] :or {D 2}}]
  (let [{:keys [merges pad]} (padding (count lst) D)]
    (loop [s (concat (make-dummies pad) lst)
           k ^Long merges]
      (if (> k 0)
        (let [[h _s] (split-at D s)
              v      (apply + (map second h))
              [s_<= s_>] (split-with #(<= (second %) v) _s)]
          (recur (concat s_<= `(~[h v]) s_>) (unchecked-dec k)))
        (first s)))))

(defn gen-code
  [tree & {:keys [code] :or {code ["0" "1"]}}]
  (let [D (count code)]
    (letfn [(walk [t s m!]
              (let [branches (first t)]
                (if (coll? branches)
                  (loop [n  0
                         _m m!]
                    (if (< n D)
                      (recur (inc n) (walk (nth branches n) (str/join [s (nth code n)]) _m))
                      _m))
                  (assoc! m! branches s))))]
      (persistent! (walk tree "" (transient {}))))))

(defn gen-code2
  "Iterative version of gen-code. Slower"
  [tree & {:keys [code] :or {code ["0" "1"]}}]
  (let [D (count code)]
    (loop [t `(~tree)
           d `(~D)
           m (transient {})
           s '("")]
      (let [branches (first (first t))
            i        (first d)]
        (cond
          (empty? t) (persistent! m)
          (= i 0) (recur (pop t) (pop d) m (pop s))
          (< i D) (let [n (- D i)]
                    (recur (conj t (nth branches n))
                           (conj (rest d) (dec i) D)
                           m
                           (conj s (nth code n))))
          (coll? branches) (recur (conj t (first branches))
                                  (conj (rest d) (dec i) D)
                                  m
                                  (conj s (first code)))
          :else (recur (pop t)
                       (pop d)
                       (assoc! m branches (str/join (reverse s)))
                       (pop s)))))))

(defn huffman-code
  [s & {:keys [code] :or {code ["0" "1"]}}]
  (let [D (count code)]
    (-> (elem-count s)
        (make-tree :D D)
        (gen-code :code code))))

;; Test
;; (huffman-code "DAVID CAMERON SETS OUT HIS FOUR EU REFORM GOALS  BUT THE EUROPEAN COMMISSION WARNS HIS BENEFIT RESTRICTIONS MAY BE ILLEGAL.")
