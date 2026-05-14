(ns automata
    (:require [clojure.test :refer [deftest is run-tests]]))

(defrecord DFA [initial-state
                accept-states
                transitions])


(defn accepts?
      (loop [input          input
             current-state  initial-state]
            (if (empty? input)
              (contains? accept-states current-state)
              (recur (rest input)
                     ((transitions current-state) (first input))))))

                  #{:q2}
                  {:q0 {\a :q1
                        \b :q0}
                   :q1 {\a :q1
                        \b :q2}
                   :q2 {\a :q2

;----------------------------------------------------------

(def dfa-2 (->DFA :q0
                  #{:q2}
                  {:q0 {\0 :q1
                        \1 :q3}
                   :q1 {\0 :q1
                        \1 :q2}
                   :q2 {\0 :q1
                        \1 :q2}
                   :q3 {\0 :q3
                        \1 :q3}}))

;----------------------------------------------------------

(def dfa-3 (->DFA :q0
                  #{:q3}
                  {:q0 {\x :q0
                        \y :q1}
                   :q1 {\x :q0
                        \y :q2}
                   :q2 {\x :q0
                        \y :q3}
                   :q3 {\x :q3
                        \y :q3}}))

(deftest test-problem3
         (is (accepts? dfa-3 "yyy"))
         (is (accepts? dfa-3 "xyxyyyx"))
         (is (accepts? dfa-3 "xxxxxyyyyy"))
         (is (accepts? dfa-3 "yyyxxxxyyy"))
         (is (not (accepts? dfa-3 "")))
         (is (not (accepts? dfa-3 "xxx")))
         (is (not (accepts? dfa-3 "yxxyxxy")))
         (is (not (accepts? dfa-3 "xyxyyxyyx"))))


(run-tests)

