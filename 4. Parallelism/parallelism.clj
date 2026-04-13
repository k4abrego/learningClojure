(ns parallelism
  (:require [clojure.core.match :refer [action-for-row]]
            [clojure.core.match.java :refer [bean-match]]))

;1.
(defn bits
      [x]
      (.bitCount (biginteger x)))

;(bits 5) ;=> 2
;(bits 31) ;=> 5
;(bits 0) ;=> 0

(defn fact-seq
      [n]
      (loop [i 1
             r 1]
            (if (> i n)
              (bits r)
              (recur (inc i)
                     (*' r i)))))

;(fact-seq 4) ;=> 2
;(fact-seq 0) ;=> 1
;(time (fact-seq 200000)) ;=> T1


(defn fact-partial
      [[start end]] ;sending 1 parameter and then separate
      (loop [i start
             r 1]
            (if (= i end)
              r
              (recur (inc i)
                     (*' r i)))))
;(fact-partial [1 7]) ;=> 720

(defn fact-ranges
      [n p]
      (partition 2
                 1
                 (concat (range 1 n (quot n p)) [(inc n)])))

(fact-ranges 1000 4)  ;=> ((1 251) (251 501) (501 751) (751 1001))

(defn fact-par
      [n]
      (let [p (.availableProcessors (Runtime/getRuntime))]
           (bits (reduce *'
                         (pmap fact-partial
                               (fact-ranges n p))))))

(time(fact-seq 200000)) ;=> 1516114   time: 13880.971459
(time(fact-par 200000)) ;=> 1516114   time: 763.471583

;Sp = (/ 13.880 0.763)
;=> 18.1913499344692
;2.



(defn create-random-data
      [n]
      (repeatedly n #(rand-int 1000)))

(create-random-data 100)

(defn insertion-sort
      [s]
      (loop [s s
             r ()]
            (if (empty? s)
              r
              (let [x (first s)
                    [before after] (split-with #(< % x) r)]
                   (recur (rest s)
                          (concat before [x] after))))))

;(apply <= (insertion-sort (create-random-data )))


(defn merge-algorithm
      [a b]
      (loop [a a
             b b
             r []]
            (cond
              (empty? a)
              (concat r b)

              (empty? b)
              (concat r a)

              (< (first a) (first b))
              (recur (rest a)
                     b
                     (conj r (first a)))
              :else
              (recur a
                     (rest b)
                     (conj r (first b))))))

;(merge-algorithm [1 4 6 9] [2 3 5 7 8 10]) ;=> (1 2 3 4 5 6 7 8 9 10)

(defn hybrid-sort-seq
      [s]
      (if (< (count s) 100)
        (insertion-sort s)
        (let [[a b] (split-at (quot (count s) 2) s)]
             (merge-algorithm (hybrid-sort-seq a)
                              (hybrid-sort-seq b)))))

(defn hybrid-sort-par
      [s]
      (if (< (count s) 100)
        (insertion-sort s)
        (let [splitted (split-at (quot (count s) 2) s)]
             (apply merge-algorithm (pmap hybrid-sort-par splitted)))))


(def n 1000)
;(time (apply <= (hybrid-sort-seq (create-random-data n))))
(time (apply <= (hybrid-sort-par (create-random-data n))))

|