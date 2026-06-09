(ns turing-machines
    (:require [clojure.test :refer [deftest is run-tests]])
    (:import (java.io Writer)))

(defrecord TM [initial-state accept-states transitions])

(defrecord Tape [left head right]
           Object
           (toString [_] (format "%s[%s]%s" left head right)))

(defmethod print-method Tape  ;use to define our tape and printed operations
           [self ^Writer writer]
           (.write writer (str self)))

(print (->Tape "aaa" \b "ccc"))

;(make-tape "aaaa") => "[a]aaa"
;(make-tape "aa" \b "cc) => "aa[b]cc"
;;(make-tape "____aaa") => "[a]aa"

(defn make-tape
      ([s]
       (let [result (drop-while #(= % \_) s)]
            (make-tape "" (if (empty? result) \_ (first result)) (rest result))))
      ([left head right]
       (let [new-left (drop-while #(= % \_) left)
             new-right (reverse (drop-while #(= % \_) (reverse right)))]
            (->Tape (apply str new-left)
                    head
                    (apply str new-right)))))
;(make-tape "") ;[_]

(defn write-tape
      [{:keys [left right]} value]
      (make-tape left value right))

;(write-tape (make-tape "aaa" \b "ccc") \x) ;=> aaa[x]ccc

(defn shift-head
      [{:keys [left head right]} direction]
      (case direction
            :left (make-tape (or (butlast left) ()) ;no salga nil
                             (or (last left) \_)
                             (str head right)) ;the new right part (join)
            :right (make-tape (str left head)
                              (or (first right) \_)
                              (rest right)) ;the "" is always the empty list
            (throw (ex-info (str "Bad direction: " direction) {}))))

;(shift-head (make-tape "aaa" \b "ccc") :right) ;=> aaab[c]cc
;(shift-head (make-tape "aaa" \b "ccc") :left) ;=> aa[a]bccc
;(shift-head (make-tape "") :left) ;=>[_]
;(shift-head (make-tape "aaaaaa") :left) ;=>[_]aaaaaa
;(shift-head (make-tape "aaaaaa") :right) ;=>a[a]aaaaa

(defn accepts
      [{:keys [initial-state accept-states transitions]} input]
      (loop [tape (make-tape input)
             current-state initial-state]
            (if (contains? accept-states current-state)
              (str tape)
              (if-let [[write-symbol direction new-state]
                       ((transitions current-state) (.head tape))]
                      (recur (shift-head (write-tape tape write-symbol) direction)
                             new-state)
                      nil))))

;----------------------------------------------------------
;Problem 1
(def tm-1
  "Accepts strings over {a} that contain an even number of a's, including the empty string"
  (->TM :q0
        #{:q2}
        {:q0 {\a [\a :right :q1] ;associated maps
              \_ [\_ :left :q2]} ;current-states
         :q1 {\a [\a :right :q0]}}))

;(accepts tm-1 "")

(deftest test-problem1
         (is (= "[_]"
                (accepts tm-1 "")))
         (is (= "a[a]"
                (accepts tm-1 "aa")))
         (is (= "aaaaaaa[a]"
                (accepts tm-1 "aaaaaaaa")))
         (is (= "aaaaaaaaaaaaaaaaaaaaaaaaa[a]"
                (accepts tm-1 "aaaaaaaaaaaaaaaaaaaaaaaaaa")))
         (is (nil? (accepts tm-1 "a")))
         (is (nil? (accepts tm-1 "aaa")))
         (is (nil? (accepts tm-1 "aaaaaaa")))
         (is (nil? (accepts tm-1 "aaaaaaaaaaaaaaaaaaaaaaaaa"))))

;----------------------------------------------------------
;Problem 2
(def tm-2
  "Accepts binary strings that start with one symbol and are followed only by the opposite symbol."
  (->TM :q0
        #{:q2}
        {:q0 {\0 [\0 :right :q1]
              \1 [\1 :right :q3]}
         :q1 {\1 [\1 :right :q1]
              \_ [\_ :left :q2]}
         :q3 {\0 [\0 :right :q3]
              \_ [\_ :left :q2]}}))

;(accepts tm-2 "10000")

(deftest test-problem2
         (is (= "[0]"
                (accepts tm-2 "0")))
         (is (= "[1]"
                (accepts tm-2 "1")))
         (is (= "1[0]"
                (accepts tm-2 "10")))
         (is (= "0111111111[1]"
                (accepts tm-2 "01111111111")))
         (is (nil? (accepts tm-2 "")))
         (is (nil? (accepts tm-2 "00")))
         (is (nil? (accepts tm-2 "100000000001")))
         (is (nil? (accepts tm-2 "10011010100101011"))))

;----------------------------------------------------------
;Problem 3
(def tm-3
  "Interprets the input as a binary number and adds one to it"
  (->TM :q0
        #{:q3}
        {:q0 {\0 [\0 :right :q0]
              \1 [\1 :right :q0]
              \_ [\_ :left :q1]}

         :q1 {\0 [\1 :right :q2]
              \1 [\0 :left :q1]
              \_ [\1 :right :q2]}

         :q2 {\0 [\0 :right :q2]
              \_ [\_ :left :q3]}}))

;(accepts tm-3 "101") ;11[0]

(deftest test-problem3
         (is (= "[1]"
                (accepts tm-3 "0")))
         (is (= "1[0]"
                (accepts tm-3 "1")))
         (is (= "1[1]"
                (accepts tm-3 "10")))
         (is (= "10[0]"
                (accepts tm-3 "11")))
         (is (= "100[1]"
                (accepts tm-3 "1000")))
         (is (= "10101011[0]"
                (accepts tm-3 "101010101")))
         (is (= "000000000[1]"
                (accepts tm-3 "0000000000")))
         (is (= "11111000[0]"
                (accepts tm-3 "111101111")))
         (is (= "101001101[1]"
                (accepts tm-3 "1010011010")))
         (is (= "1000000000000000[0]"
                (accepts tm-3 "1111111111111111"))))

;----------------------------------------------------------
;4
(def tm-4
  "Processes strings of the form a*$a* and removes matching a's from both sides of the $"
  (->TM :q0
        #{:q7}
        {:q0 {\a [\a :right :q0]
              \$ [\$ :right :q1]}
         :q1 {\a [\a :right :q1]
              \_ [\_ :left :q2]}
         :q2 {\a [\_ :left :q3]
              \$ [\_ :left :q7]}
         :q3 {\a [\a :left :q3]
              \$ [\$ :left :q4]}
         :q4 {\a [\a :left :q4]
              \_ [\_ :right :q5]}
         :q5 {\a [\_ :right :q0]
              \$ [\_ :right :q6]}
         :q6 {\a [\_ :right :q6]
              \_ [\_ :right :q7]}}))

;(accepts tm-4 "$")

(deftest test-problem4
         (is (= "[_]"
                (accepts tm-4 "$")))
         (is (= "[_]"
                (accepts tm-4 "a$a")))
         (is (= "[a]"
                (accepts tm-4 "aa$a")))
         (is (= "aa[a]"
                (accepts tm-4 "aaaaa$aa")))
         (is (= "[_]"
                (accepts tm-4 "aaaaa$aaaaaaaa")))
         (is (= "aa[a]"
                (accepts tm-4 "aaaaaaaa$aaaaa")))
         (is (= "[_]"
                (accepts tm-4 "$aaaaaaaaaaaaa")))
         (is (= "aaaaaaaaaaaa[a]"
                (accepts tm-4 "aaaaaaaaaaaaa$"))))

;----------------------------------------------------------
;5
(def tm-5
  "Accepts strings of the form a^n b^n c^n, where n is greater than or equal to zero"
  (->TM :q0
        #{:q10}
        {:q0 {\a [\a :right :q0]
              \b [\b :left :q1]
              \_ [\_ :left :q9]}
         :q1 {\a [\b :right :q1]
              \b [\b :right :q1]
              \c [\c :left :q2]}
         :q2 {\b [\c :left :q3]}
         :q3 {\b [\c :right :q3]
              \c [\c :right :q3]
              \_ [\_ :left :q4]}
         :q4 {\c [\_ :left :q5]}
         :q5 {\c [\_ :left :q6]}
         :q6 {\c [\_ :left :q7]}
         :q7 {\a [\a :left :q7]
              \b [\b :left :q7]
              \c [\c :left :q7]
              \_ [\_ :right :q8]}
         :q8 {\a [\a :right :q0]
              \_ [\_ :right :q10]}
         :q9 {\_ [\_ :right :q10]}}))


;(accepts tm-5 "") ;=> "[_]"
;(accepts tm-5 "abc") ;=> "[_]"
;(accepts tm-5 "aaaaaaaaaabbbbbbbbbbcccccccccc") ;=> "[_]"
;(accepts tm-5 "aaaaaaaaaabbbbbbbbbcccccccccc") ;=> nil
;(accepts tm-5 "a") ;=> nil


;5
(deftest test-problem5
         (is (accepts tm-5 ""))
         (is (accepts tm-5 "abc"))
         (is (accepts tm-5 "aaabbbccc"))
         (is (accepts tm-5 "aaaaaaaaaabbbbbbbbbbcccccccccc"))
         (is (nil? (accepts tm-5 "a")))
         (is (nil? (accepts tm-5 "aabbc")))
         (is (nil? (accepts tm-5 "aabaca")))
         (is (nil? (accepts tm-5 "cccaaabbb")))
         (is (nil? (accepts tm-5 "aaaaaccccc")))
         (is (nil? (accepts tm-5 "abcabcabcabc")))
         (is (nil? (accepts tm-5 "aaaaabbbbbcccccc")))
         (is (nil? (accepts tm-5 "aaaaaaaaaabbbbbbbbbcccccccccc"))))

;----------------------------------------------------------
;6.
(def tm-6
  "Accepts binary strings that contain the same number of 0's and 1's"
  (->TM :q0
        #{:q10}
        {:q0 {\0 [\0 :right :q0]
              \1 [\1 :right :q0]
              \_ [\_ :left :q1]}
         :q1 {\0 [\_ :left :q2]
              \1 [\_ :left :q3]
              \_ [\_ :left :q9]}
         :q2 {\0 [\0 :left :q2]
              \1 [\1 :left :q2]
              \_ [\_ :left :q4]}
         :q3 {\0 [\0 :left :q3]
              \1 [\1 :left :q3]
              \_ [\_ :left :q5]}
         :q4 {\0 [\0 :left :q4]
              \1 [\1 :left :q6]
              \_ [\0 :right :q8]}
         :q5 {\1 [\1 :left :q5]
              \0 [\0 :left :q6]
              \_ [\1 :right :q8]}
         :q6 {\0 [\0 :left :q6]
              \1 [\1 :left :q6]
              \_ [\_ :right :q7]}
         :q7 {\0 [\_ :right :q8]
              \1 [\_ :right :q8]}
         :q8 {\0 [\0 :right :q8]
              \1 [\1 :right :q8]
              \_ [\_ :right :q0]}
         :q9 {\_ [\_ :right :q10]}}))


;(accepts tm-6 "11")

(deftest test-problem6
         (is (accepts tm-6 ""))
         (is (accepts tm-6 "01"))
         (is (accepts tm-6 "10"))
         (is (accepts tm-6 "11000101"))
         (is (accepts tm-6 "1010011010"))
         (is (accepts tm-6 "1010101010101010"))
         (is (accepts tm-6 "1111111100000000"))
         (is (accepts tm-6 "00000111111111100000"))
         (is (nil? (accepts tm-6 "11")))
         (is (nil? (accepts tm-6 "01010")))
         (is (nil? (accepts tm-6 "11111111000000001")))
         (is (nil? (accepts tm-6 "10101111001101110101")))
         (is (nil? (accepts tm-6 "000000000000000000000")))
         (is (nil? (accepts tm-6 "11111111110111111111111"))))



(run-tests)