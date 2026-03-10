(ns simple-examples)

(defn fact-v1
  [n]
  (if (zero? n)
    1
    (*' n (fact-v1 (dec n)))))

(fact-v1 4)

(defn fact-v2
  [n]
  (loop [i 1
         result 1]
    (if (> i n)
      result
      (recur (inc i) ;back to the loop
             (*' result i)))))

(fact-v2 0)
(fact-v2 1)
(fact-v2 5)
(fact-v2 1000)
(fact-v2 10000)
(fact-v2 4)


;range
(defn fact-v3
  [n]
  (reduce *' (range 1 (inc n)))) ;(reduce f val coll) / (range start end)

(fact-v3 0)
(fact-v3 1)
(fact-v3 5)
(fact-v2 1000)
(fact-v2 10000)