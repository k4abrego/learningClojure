(ns automata)

(ns automata
    (:require [clojure.test :refer [deftest is run-tests]]))

(defrecord DFA [initial-state
                accept-states
                transitions])

;----------------------------------------------------------
; Problem 1

(defn accepts?
      [{:keys [initial-state accept-states transitions]} input]
      (loop [input          input
             current-state  initial-state]
            (if (empty? input)
              (contains? accept-states current-state)
              (recur (rest input)
                     ((transitions current-state) (first input))))))

(def dfa-1 (->DFA :q0
                  #{:q2}
                  {:q0 {\a :q1
                        \b :q0}
                   :q1 {\a :q1
                        \b :q2}
                   :q2 {\a :q2
                        \b :q2}}))

;----------------------------------------------------------
; Problem 2

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
; Problem 3

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

;----------------------------------------------------------
; Problem 4
(def dfa-4 (->DFA :q0
                  #{:q0}
                  { :q0 {\i :q1
                         \j :q1
                         \k :q1}
                    :q1 {\i :q0
                         \j :q0
                         \k :q0}}))


;----------------------------------------------------------
; Problem 5
(def dfa-5 (->DFA :q0
                  #{:q0}
                  {

                   }))

;----------------------------------------------------------
; Tests

(deftest test-problem1
         (is (accepts? dfa-1 "ab"))
         (is (accepts? dfa-1 "abba"))
         (is (accepts? dfa-1 "aaab"))
         (is (accepts? dfa-1 "abbbbbbbbb"))
         (is (not (accepts? dfa-1 "")))
         (is (not (accepts? dfa-1 "a")))
         (is (not (accepts? dfa-1 "baa")))
         (is (not (accepts? dfa-1 "bbba"))))

(deftest test-problem2
         (is (accepts? dfa-2 "01"))
         (is (accepts? dfa-2 "0101"))
         (is (accepts? dfa-2 "01111"))
         (is (accepts? dfa-2 "000001"))
         (is (not (accepts? dfa-2 "")))
         (is (not (accepts? dfa-2 "00")))
         (is (not (accepts? dfa-2 "1001011")))
         (is (not (accepts? dfa-2 "1001010"))))

(deftest test-problem3
         (is (accepts? dfa-3 "yyy"))
         (is (accepts? dfa-3 "xyxyyyx"))
         (is (accepts? dfa-3 "xxxxxyyyyy"))
         (is (accepts? dfa-3 "yyyxxxxyyy"))
         (is (not (accepts? dfa-3 "")))
         (is (not (accepts? dfa-3 "xxx")))
         (is (not (accepts? dfa-3 "yxxyxxy")))
         (is (not (accepts? dfa-3 "xyxyyxyyx"))))

(deftest test-problem4
         (is (accepts? dfa-4 ""))
         (is (accepts? dfa-4 "ji"))
         (is (accepts? dfa-4 "iiiijjjjkkkk"))
         (is (accepts? dfa-4 "kjikjikjikjikjikjikjikji"))
         (is (not (accepts? dfa-4 "i")))
         (is (not (accepts? dfa-4 "ijk")))
         (is (not (accepts? dfa-4 "jjjjjiiiiikkkkk")))
         (is (not (accepts? dfa-4
                            "kjikjikjikjikjikjikjikjikji"))))

(run-tests)



