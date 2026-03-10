;----------------------------------------------------------
; Problem Set #1: Introductory Exercises
; Date: February 20, 2026.
; Authors:
;          A01753979 Ana Karen Abrego Flores
;          A01803514 Gabriel de Jesús Manzo Cuevas
;----------------------------------------------------------
(ns introductory
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

;----------------------------------------------------------
; 1.

(defn gibibytes->bytes
  "convert gibibytes to the corresponding number of bytes."
  [gibibytes]
  (* gibibytes 1024 1024 1024))

;(gibibytes->bytes 5)

(deftest test-gibibytes->bytes
  (is (= 0 (gibibytes->bytes 0)))
  (is (= 1073741824 (gibibytes->bytes 1)))
  (is (= 5368709120 (gibibytes->bytes 5)))
  (is (= 26415122612224 (gibibytes->bytes 24601))))

;----------------------------------------------------------
; 2.

(defn fahrenheit->celsius
  "convert fahrenheit to celsius"
  [f]
  (/ (* 5
        (- f 32.0))
     9.0))

; (fahrenheit->celsius 212)
(deftest test-fahrenheit->celsius
  (is (= 100.0 (fahrenheit->celsius 212.0)))
  (is (= 0.0 (fahrenheit->celsius 32.0)))
  (is (= -40.0 (fahrenheit->celsius -40.0))))


;----------------------------------------------------------
; 3.
(defn sign
  "Determines if n is positive (1), negative (-1) or zero (0)"
  [n]
  (cond
    (neg? n) -1
    (pos? n) 1
    :else 0))

;(sign -5)
(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))
;----------------------------------------------------------
; 4.
(defn roots
  "computes the 2 roots to solve:
  ax2 + bx + c"
  [a b c]
  (let [d (- b)
        e (sqrt (- (* b b)
                   (* 4 a c)))
        f (* 2 a)
        x1 (/ (+ d e) f)
        x2 (/ (- d e) f)]
    [x1 x2]))

(roots 2 4 2)

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))
;----------------------------------------------------------
; 5.
(defn bmi
  "It calculates the BMI and indicates whether a person's weight and height proportion is adequate"
  [weight height]
  (let [b (/ weight (* height height))]
    (if (< b 20)
      'underweight
      (if (< b 25)
        'normal
        (if (< b 30)
          'obese1
          (if (< b 40)
            'obese2
            'obese3))))))

(bmi 120 1.6)
(deftest test-bmi
 (is (= 'underweight (bmi 45 1.7)))
(is (= 'normal (bmi 55 1.5)))
(is (= 'obese1 (bmi 76 1.7)))
 (is (= 'obese2 (bmi 81 1.6)))
(is (= 'obese3 (bmi 120 1.6))))
;----------------------------------------------------------
; 6.
(defn type-of-triangle
  "Determine the type of triangle it is"
  [a b c]
  (if (= a b c)
    'equilateral
    (if (or (= a b) (= a c) (= b c))
      'isosceles
      'scalene)))

(type-of-triangle 1 2 3)

(deftest test-type-of-triangle
  (is (= 'equilateral (type-of-triangle 3 3 3)))
  (is (= 'equilateral (type-of-triangle 4.2 4.2 4.2)))
  (is (= 'isosceles (type-of-triangle 4 4 3)))
  (is (= 'isosceles (type-of-triangle 4 3 4)))
  (is (= 'isosceles (type-of-triangle 3 4 4)))
  (is (= 'scalene (type-of-triangle 1 2 3)))
  (is (= 'scalene (type-of-triangle 7.1 6.4 9.2))))

(run-tests) ;load file in REPL