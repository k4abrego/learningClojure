(ns turing-machines
    (:require [clojure.test :refer [deftest is run-tests]])
    (:import (com.jetbrains.cef.remote.thrift TApplicationException)
      (java.io Writer)))

(defrecord TM [initial-state accept-states transitions])

(defrecord Tape [left head right]
           Object
           (toString [_] (format "%s[%s]%s" left head right)))

(defmethod print-method Tape  ;use to define our tape and printed operations
           [self ^Writer writer]
           (.writer writer (str self)))

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
            :right ((make-tape (str left head)
                               (or (first right) \_)
                               (rest right))) ;the "" is always the empty list
            (throw (ex-info (str "Bad direction: " direction) {}))))

;(shift-head (make-tape "aaa" \b "ccc") :right) ;=> aaab[c]cc
;(shift-head (make-tape "aaa" \b "ccc") :left) ;=> aa[a]bccc
;(shift-head (make-tape "") :left) ;=>[_]
;(shift-head (make-tape "aaaaaa") :left) ;=>[_]aaaaaa
;(shift-head (make-tape "aaaaaa") :right) ;=>a[a]aaaaa

(def accepts
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

;Problem 1
(def tm-1 (->TM :q0
                #{:q2}
                {:q0 {\a [\a :right :q1] ;associated maps
                      \_ [\_ :left :q2]} ;current-states
                 :q1 {\a [\a :right :q0]}}))

(accepts tm-1 "")

;Problem 2



;tests
;1
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
;2


(run-tests)