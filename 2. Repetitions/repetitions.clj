;----------------------------------------------------------
; Problem Set #2: Repetitions
; Date: March 3, 2025.
; Authors:
;          A01753979 Ana Karen Abrego Flores
;          A01803514 Gabriel de Jesús Manzo Cuevas
;----------------------------------------------------------

(ns repetitions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]])
  (:require [clojure.algo.generic.math-functions
             :refer [sqr approx=]]))

;----------------------------------------------------------
; 1.
;-- recursive version --
;(defn enlist
;  [s]
;  (if (empty? s)
;    () ;return an empty list
;    (cons (list (first s)) ;agregamos 1
;          (enlist (rest s))))) ; ((1) (2) (3) (4))

;-- loop version --
;(defn enlist
;  [s]
;  (loop [s s  ;
;         result []] ;using a vector to join the values
;    (if (empty? s)
;      ;(reverse result)
;      (seq result)  ;para que => ((1) (2) (3) (4))
;      (recur (rest s) ;
;             (conj result
;               (list (first s)))))))

;-- sequence API --
(defn enlist
  [s]
  (map list s))

(enlist [1 2 3 4])
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '((1) (2) (3) (4)) (enlist [1 2 3 4])))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

;----------------------------------------------------------
;2.

(defn positives
  [s]
  (filter pos? s))

(positives [-4 3 -1 -10 -13 6 -5])

(deftest test-positives
  (is (= () (positives ())))
  (is (= () (positives [-4 -1 -10 -13 -5])))
  (is (= [3 6] (positives [-4 3 -1 -10 -13 6 -5])))
  (is (= [4 3 1 10 13 6 5] (positives [4 3 1 10 13 6 5]))))


;----------------------------------------------------------
;3.
;map just to return the squares and reduce to return the sum of each
(defn add-squares
  [s]
  (reduce + (map sqr s)))

(add-squares[2 4 1 3])

(deftest test-add-squares
  (is (= 0 (add-squares [])))
  (is (= 25 (add-squares [5])))
  (is (= 30 (add-squares [2 4 1 3])))
  (is (= 385 (add-squares [1 2 3 4 5 6 7 8 9 10]))))


;----------------------------------------------------------
;4.
;-- recursive solution --
;(defn duplicate
;  [s]
;  (if (empty? s)
;    ()
;    (cons (first s)
;          (cons (first s)
;                (duplicate (rest s))))))

;-- loop solution --
;(defn duplicate
;  [s]
;  (loop [s s ;'(a b c)
;         result []]
;    (if (empty? s)
;      result
;      ;(seq result)
;      (recur (rest s)
;             (conj (conj result
;                         (first s))
;                   (first s))))))

;-- sequence API --
;(defn dup
;  [x]
;  (list x x))
(defn duplicate
  [s]
  ;(reduce concat (map dup s)))
  ;(mapcat dup s))
  (interleave s s));interleave if ypu use the same list twice

(duplicate '(1 2 3))

;(deftest test-duplicate
;  (is (= [1 1 2 2 3 3 4 4 5 5]
;         (duplicate [1 2 3 4 5])))
;  (is (= ()
;         (duplicate ())))
;  (is (= '(a a)
;         (duplicate '(a))))
;  (is (= '(a a b b c c d d e e f f g g h h)
;         (duplicate '(a b c d e f g h)))))

;----------------------------------------------------------
;5.

;-- recursive solution --
;(defn fib
;  [n]
;  (if (<= n 1)
;    n
;    (+ (fib (- n 1))
;       (fib (- n 2)))))

;-- loop solution --
;(defn fib
;  [n]
;  (loop [a 0
;    b 1
;    i 0]
;    (if (= i n)
;      a
;      (recur b
;             (+' a b)
;             (inc i)))))

;-- sequence API --
;all the parameters of n
;(defn fib
;  [n]
;  (take n (iterate (fn [param]
;           (let [a (first param)
;                 b (second param)]
;             [b (+' a b)]))
;         [0 1])))

;drop

;(defn fib
;  [n]
;  (first (drop n (iterate (fn [param]
;                     (let [a (first param)
;                           b (second param)]
;                       [b (+' a b)]))
;                   [0 1]))))

;only the last first param
(defn fib
  [n]
  (first
    (first (drop n (iterate (fn [param]
                              (let [a (first param)
                                    b (second param)]
                                [b (+' a b)]))
                            [0 1])))))
(fib 5)

(deftest test-fib
  (is (= 0
         (fib 0)))
  (is (= 1
         (fib 1)))
  (is (= 1
         (fib 2)))
  (is (= 5
         (fib 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
          987 1597 2584 4181 6765]
         (map fib (range 21))))
  (is (= 267914296
         (fib 42))))


;----------------------------------------------------------
;6.
;(defn pow
;  [a b]
;  (loop [accum 1
;         n   b]
;    (if (= n 0)
;      accum
;      (recur (*' accum a) (dec n)))))

(defn pow
  "returns a raised to the power b using simple recursion"
  [a b]
  (if (= b 0)
    1
    (*' a (pow a (- b 1)))))

(pow 2 100)

(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22)))
  (is (= 1267650600228229401496703205376N (pow 2 100))))

;----------------------------------------------------------
;7.

(defn only-symbols?
  "returns true if every element in s is a symbol"
  [s]
  (every? symbol? s))

(only-symbols? '(a))

(deftest test-only-symbols?
  (is (= true (only-symbols? [])))
  (is (= true (only-symbols? '(a))))
  (is (= true (only-symbols? '(a b c d e))))
  (is (= false (only-symbols? '(a b c d 42 e))))
  (is (= false (only-symbols? '(42 a b c))))
  (is (= false (only-symbols? [4 8 15 16 23 42]))))

;----------------------------------------------------------
;8.
;loop
;(defn invert-pairs
;  [s]
;  (map (fn [v]
;         (let [a (first v)
;               b (second v)]
;           [b a]))
;       s))

;API
(defn invert-pairs
  "takes a sequence s as its input and tt returns true
  if all the elements in s are symbols"
  [s]
  (map (fn [[a b]] [b a]) s))

(invert-pairs '([a 1][a 2][b 1][b 2]))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([y x]) (invert-pairs '([x y]))))
  (is (= '([1 a][2 a][1 b][2 b])
         (invert-pairs '([a 1][a 2][b 1][b 2]))))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

;----------------------------------------------------------
;9.
(defn replic
  "replicates each element of s exactly n times and returns a
  sequence of the repeated elements"
  [n s]
  (mapcat #(repeat n %) s))

(replic 3 '(a)); => (a a a)

(deftest test-replic
  (is (= () (replic 7 [])))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= [1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4]
         (replic 4 [1 2 3 4]))))


;----------------------------------------------------------
;10.
(defn dot-product
  "it returns the dot product of two numeric sequences.
  multiplies corresponding elements and sums the results."
  [a b]
  (reduce +(map * a b)))

(dot-product [6] [7])

(deftest test-dot-product
  (is (= 0 (dot-product [] [])))
  (is (= 42 (dot-product [6] [7])))
  (is (= 32 (dot-product [1 2 3] [4 5 6])))
  (is (= 21.45 (dot-product [1.3 3.4 5.7 9.5 10.4]
                            [-4.5 3.0 1.5 0.9 0.0]))))

;----------------------------------------------------------
;11.
(defn average
  "takes a sequence of numbers, it returns the arithmetic mean
  of the numbers contained in, or nil if is empty"
  [s]
  (if (empty? s)
    nil
    (/ (reduce + s)
       (count s))))

(average [5 6 1 6 0 1 2])

(deftest test-average
  (is (nil? (average [])))
  (is (= 4
         (average [4])))
  (is (= 3
         (average [5 6 1 6 0 1 2])))
  (is (= 2.5
         (average [1.7 4.5 0.0 2.0 3.4 5.0 2.5 2.2 1.2]))))

;----------------------------------------------------------
;12.
(defn standard-deviation
  "returns the standard deviation of numeric sequence s, and
  returns nil if the sequence is empty"
  [s]
  (if (seq s)
    (let [avg (average s)
          sum (reduce +'
                      (map (fn [x]
                             (let [dif (- x avg)]
                               (* dif dif)))
                           s))
          n   (count s)]
      (sqrt (/ sum n)))
    nil))

(standard-deviation [4 8 15 16 23 42])

(deftest test-standard-deviation
  (is (nil? (standard-deviation [])))
  (is (approx= 1.87
               (standard-deviation [6 2 3 1])
               0.01))
  (is (approx= 12.3153
               (standard-deviation [4 8 15 16 23 42])
               0.0001))
  (is (approx= 7.07106
               (standard-deviation [110 105 90 100 95])
               0.00001))
  (is (approx= 2.983
               (standard-deviation [9 2 5 4 12 7 8 11
                                    9 3 7 4 12 5 4 10
                                    9 6 9 4])
               0.001)))

(run-tests)
