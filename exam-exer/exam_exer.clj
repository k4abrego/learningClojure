(ns exam-exer
    (:require [clojure.test :refer [deftest is run-tests]]))

(defrecord DFA [initial-state
                accept-states
                transitions])

;----------------------------------------------------------
;AUTOMATA

(defn accepts?
      [{:keys [initial-state accept-states transitions]} input]
      (loop [input          input
             current-state  initial-state]
            (if (empty? input)
              (contains? accept-states current-state)
              (recur (rest input)
                     ((transitions current-state) (first input))))))
(def dfa-contains-yyy
  (->DFA :q0
         #{:q3}
         {:q0 {\x :q0
               \y :q1}
          :q1 {\x :q0
               \y :q2}
          :q2 {\x :q0
               \y :q3}
          :q3 {\x :q3
               \y :q3}}))

(is (accepts? dfa-contains-yyy "yyxxxxyyy"))


(def dfa-one-m
  (->DFA :q0
         #{:q1}
         {:q0 {\m :q1
               \n :q0}
          :q1 {\m :q2
               \n :q1}
          :q2 {\m :q2
               \n :q2}}))

(is (accepts? dfa-one-m "n"))

(def dfa-even-x
  (->DFA :q0
         #{:q0}
         {:q0 {\x :q1
               \o :q0}
          :q1 {\x :q0
               \o :q1}}))

(is (accepts? dfa-even-x "xoxo"))


(def dfa-end-xo
  (->DFA :q0
         #{:q2}
         {:q0 {\x :q1
               \o :q0}
          :q1 {\x :q1
               \o :q2}
          :q2 {\x :q1
               \o :q0}}))

(is (accepts? dfa-even-x "oxoxooxoxo"))


;----------------------------------------------------------
;REGEX