(ns parallelism)

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

;2.












