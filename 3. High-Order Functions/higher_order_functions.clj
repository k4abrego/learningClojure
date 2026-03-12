(ns higher-order-functions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.algo.generic.math-functions
             :refer [approx=]]))

;1.
(defn argswap
      [fun]
      (fn [x y] (fun y x)))

((argswap -) 10 3) ;=> -7
((argswap cons) [1 2 3 4] 5) ;=> (5 1 2 3 4)

(deftest test-argswap
         (is (= '(2 1)
                ((argswap list) 1 2)))
         (is (= -7
                ((argswap -) 10 3)))
         (is (= 1/4
                ((argswap /) 8 2)))
         (is (= '((4 5 6) 1 2 3)
                ((argswap cons) '(1 2 3) '(4 5 6))))
         (is (= '(1 0 4 25 100)
                ((argswap map) '(-1 0 2 5 10) #(* % %)))))

;----------------------------------------------------------
;2.
(defn there-exists-one
      [pred? s]
      (= 1 (count (filter pred? s))))

(there-exists-one zero? [4 3 1 10 5 1]) ;=> false
(there-exists-one zero? [4 3 1 10 0 1]) ;=> true
(there-exists-one zero? [4 3 0 1 10 0 1]) ;=> false

(deftest test-there-exists-one
         (is (not (there-exists-one pos?
                                    ())))
         (is (there-exists-one pos?
                               '(-1 -10 4 -5 -2 -1)))
         (is (there-exists-one neg?
                               '(-1)))
         (is (not (there-exists-one symbol?
                                    '(4 8 15 16 23 42))))
         (is (there-exists-one symbol?
                               '(4 8 15 sixteen 23 42))))

;----------------------------------------------------------
;3.
(defn linear-search
      [vct x  eq-fun]
      (loop [i 0]
            (cond
              (= i (count vct))     nil
              (eq-fun  x (vct i))   i
              :else                 (recur (inc i)))))

(linear-search [4 8 15 16 23 42] 16 =) ;3
(linear-search [4 8 15 16 23 42] 17 =) ;nil
(linear-search [4 8 15 16 23 42] 15.0 ==) ;2
(linear-search [4 8 15 16 23 42] 22 (fn [a b] (<= (abs (- a b)) 1))) ;4 cause of the comparing function
(linear-search [4 8 15 16 23 42] 1
               (fn [a b] (= (first (str a))
                            (first (str b)))))
(linear-search
  ["red" "blue" "green" "black" "white"]
  "black"
  identical?) ;=> 3

(linear-search
  [48 77 30 31 5 20 91 92
   69 97 28 32 17 18 96]
  27
  #(<= (abs (- %1 %2)) 1))

(deftest test-linear-search
         (is (nil? (linear-search [] 5 =)))
         (is (= 0 (linear-search [5] 5 =)))
         (is (= 4 (linear-search
                    [48 77 30 31 5 20 91 92
                     69 97 28 32 17 18 96]
                    5
                    =)))
         (is (= 3 (linear-search
                    ["red" "blue" "green" "black" "white"]
                    "black"
                    identical?)))
         (is (nil? (linear-search
                     [48 77 30 31 5 20 91 92
                      69 97 28 32 17 18 96]
                     96.0
                     =)))
         (is (= 14 (linear-search
                     [48 77 30 31 5 20 91 92
                      69 97 28 32 17 18 96]
                     96.0
                     ==)))
         (is (= 8 (linear-search
                    [48 77 30 31 5 20 91 92
                     69 97 28 32 17 18 96]
                    70
                    #(<= (abs (- %1 %2)) 1)))))

;----------------------------------------------------------
;4.


;----------------------------------------------------------
;5.


;----------------------------------------------------------
;6.


;----------------------------------------------------------
;7.



(run-tests)