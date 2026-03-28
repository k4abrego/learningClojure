;----------------------------------------------------------
; Problem Set #4: Higher-Order Functions
; Date: March 27, 2026.
; Authors:
;          A01753979 Ana Karen Abrego Flores
;          A01803514 Gabriel de Jesús Manzo Cuevas
;----------------------------------------------------------

(ns higher-order-functions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.algo.generic.math-functions
             :refer [approx=]]))

;1.
(defn argswap
      "returns a new function that calls fun with its two arguments swapped"
      [fun]
      (fn [x y] (fun y x)))

((argswap -) 10 3) ;=> -7
((argswap cons) [1 2 3 4] 5) ;=> (5 1 2 3 4)

;----------------------------------------------------------
;2.
(defn there-exists-one
      "returns true if exactly one element in collection s satisfies predicate pred?, otherwise false"
      [pred? s]
      (= 1 (count (filter pred? s))))

(there-exists-one zero? [4 3 1 10 5 1]) ;=> false
(there-exists-one zero? [4 3 1 10 0 1]) ;=> true
(there-exists-one zero? [4 3 0 1 10 0 1]) ;=> false

;----------------------------------------------------------
;3.
(defn linear-search
      "performs a linear search on vector vct to find element x using eq-fun for comparison. returns the index or nil if not found"
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

;----------------------------------------------------------
;4.
(defn deriv
      "returns a function that approximates the derivative of f using a finite difference with step size h"
      [f h]
      (fn [x]
          (/ (- (f (+ x h))
                (f x))
             h)))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

;----------------------------------------------------------
;5.
(defn newton
      "applies Newton's method recursively n times to approximate a root of function f"
      [f n]
      (if (= n 0)
      0
      (let [prev (newton f (- n 1))
            df (deriv f 0.0001)]
           (- prev (/ (f prev) (df prev))))))

(newton (fn [x] (+ (* 4 x) 2)) 1) ;=> -0.5000000000000551

;----------------------------------------------------------
;6.
(defn integral
      "approximates the definite integral of function f from a to b using Simpson's rule with n subdivisions"
      [a b n f]
      (let [h (/ (- b a) n)]
           (loop [k 0
                  suma 0]
                 (if (> k n)
                   (* (/ h 3) suma)
                   (let [x (+ a (* k h))
                         y (f x)
                         coeff (if (or (= k 0) (= k n))
                                 1
                                 (if (odd? k) 4 2))]
                        (recur (+ k 1) (+ suma (* coeff y))))))))

(integral 1 2 10
          (fn [x]
              (integral 3 4 10
                        (fn [y]
                            (* x y))))) ;=> 21/4

(integral 0 1 10 (fn [x] (* x x x))) ;=> 1/4

;----------------------------------------------------------
;7.
(defn binary-search
      "performs binary search on a sorted vector to find element x using comparison function lt-fun and returns index or nil if not found"
      [vct x lt-fun]
      (loop [low 0
             high (- (count vct) 1)]
            (if (> low high)
              nil
              (let [mid (quot (+ low high) 2)
                    y (vct mid)]
                   (cond
                     (not (or (lt-fun x y) (lt-fun y x)))
                     mid
                     (lt-fun x y)
                     (recur low (- mid 1))
                     :else
                     (recur (+ mid 1) high))))))

(def small-list [4 8 15 16 23 42])

(def big-list [0 2 5 10 11 13 16 20 24 26
               29 30 31 32 34 37 40 43 44
               46 50 53 58 59 62 63 66 67
               70 72 77 79 80 83 85 86 94
               95 96 99])

(def animals ["dog" "dragon" "horse" "monkey" "ox"
              "pig" "rabbit" "rat" "rooster" "sheep"
              "snake" "tiger"])
(defn str<
      "Returns true if a is less than b, otherwise
       returns false. Designed to work with strings."
      [a b]
      (< (compare a b) 0))

(binary-search [] 5 <) ;=> nil
(binary-search small-list 42 <) ;=>5
(binary-search big-list 43 <) ;=> 17
(binary-search animals "tiger" str<) ;=> 11


;----------------------------------------------------------
;tests
;1.
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
;2.
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
;3.
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
;4.
(deftest test-deriv
         (is (approx= 75 (df 5) 0.05))
         (is (approx= 30 (ddf 5) 0.05))
         (is (approx= 6 (dddf 5) 0.05)))

;5.
(deftest test-newton
         (is (approx= 10.0
                      (newton (fn [x] (- x 10))
                              1)
                      0.00001))
         (is (approx= -0.5
                      (newton (fn [x] (+ (* 4 x) 2))
                              1)
                      0.00001))
         (is (approx= -1.0
                      (newton (fn [x] (+ (* x x x) 1))
                              50)
                      0.00001))
         (is (approx= -1.02987
                      (newton (fn [x] (+ (Math/cos x)
                                         (* 0.5 x)))
                              5)
                      0.00001)))

;6.
(deftest test-integral
         (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
         (is (= 21/4
                (integral 1 2 10
                          (fn [x]
                              (integral 3 4 10
                                        (fn [y]
                                            (* x y))))))))

;7.
(deftest test-binary-search
         (is (nil? (binary-search [] 5 <)))
         (is (= 3 (binary-search small-list 16 <)))
         (is (= 0 (binary-search small-list 4 <)))
         (is (= 5 (binary-search small-list 42 <)))
         (is (nil? (binary-search small-list 7 <)))
         (is (nil? (binary-search small-list 2 <)))
         (is (nil? (binary-search small-list 99 <)))
         (is (= 17 (binary-search big-list 43 <)))
         (is (= 0 (binary-search big-list 0 <)))
         (is (= 39 (binary-search big-list 99 <)))
         (is (nil? (binary-search big-list 12 <)))
         (is (nil? (binary-search big-list -1 <)))
         (is (nil? (binary-search big-list 100 <)))
         (is (= 5 (binary-search animals "pig" str<)))
         (is (= 0 (binary-search animals "dog" str<)))
         (is (= 11 (binary-search animals "tiger" str<)))
         (is (nil? (binary-search animals "elephant" str<)))
         (is (nil? (binary-search animals "alligator" str<)))
         (is (nil? (binary-search animals "unicorn" str<))))

(run-tests)