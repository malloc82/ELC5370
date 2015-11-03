(ns elc5370.problem5
  (:use clojure.core
        elc5370.binomial
        elc5370.patch))

(defn w [k & {:keys [lambda] :or {lambda 5.1}}]
  (let [p (fn [x] (/ (* (Math/exp (- lambda)) (Math/pow lambda (- x 2)))
                    (fact-cache (- x 2))))]
   (if (coll? k)
     (map p k)
     (p k))))
