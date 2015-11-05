(ns elc5370.problem5
  (:use clojure.core
        elc5370.binomial
        elc5370.patch))

(def N 1000000) ;; total number of english words

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
            ;; (println "sum = " s)
            {:letters (mapv first v)
             :index   (range (count v))
             :Pr      (mapv #(/ % s) p)}))

(defn w [k & {:keys [lambda] :or {lambda 6.1}}]
  (let [p (fn [x] (/ (* (Math/exp (- lambda)) (Math/pow lambda (- x 2)))
                    (fact-cache (- x 2))))]
   (if (coll? k)
     (map p k)
     (p k))))

(defn Pr_E|K
  [Pr_space k & {:keys [N] :or {N 1000000}}]
  (* (/ (* (w k) N) (Math/pow 26 (dec k)))
     (Math/pow (- 1 Pr_space) (dec k))
     Pr_space))

(defn Pr_E
  [Pr_space & {:keys [K] :or {K 100}}]
  (let [Pr_letter (- 1.0 Pr_space)]
   (+ (* (/ 3 26) Pr_letter Pr_space)
      (* (/ 114 (Math/pow 26 2)) (Math/pow Pr_letter 2) Pr_space)
      (* (/ 172 (Math/pow 26 3)) (Math/pow Pr_letter 3) Pr_space)
      (loop [k 5
             s 0.0]
        (if (<= k K)
          (recur (inc k) (+ s (Pr_E|K Pr_space k)))
          s)))))

(defn p
  [x & {:keys [a] :or {a 0.83495}}]
  (if (coll? x)
    (map #(* (- 1 a) (Math/pow a %)) x)
    (* (- 1 a) (Math/pow a x))))

(defn E_l
  [p & {:keys [K] :or {K 1000}}]
  (let [q (- 1 p)]
    (loop [k 2
           s 0.0]
      (if (<= k K)
        (recur (inc k)  (+ s (* k (Math/pow q (dec k)) p)))
        s))))
