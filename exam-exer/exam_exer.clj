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

;Define un regex llamado ends-with-st que acepte strings sobre {s,t}
;que terminan en st.

(def ends-with-st #"[st]*st")

(re-matches ends-with-st "sttstststst")



;Define un regex llamado contains-at que acepte strings sobre {#, $, @}
;que contienen al menos un @ .

(def contains-at #"[#$@]*@[#$@]*")

(re-matches contains-at "##$@")

;Define un regex llamado exactly-three-digits que
;acepte strings con exactamente tres dígitos.

(def exactly-three-digits #"\d{3}")

(re-matches exactly-three-digits "12")

;Define un regex llamado one-or-more-digits que acepte uno o más dígitos.
(def one-or-more-digits #"\d+")

(re-matches one-or-more-digits "12")


;Define un regex llamado optional-sign-integer que acepte un
;entero con signo opcional + o -

(def optional-sign-integer #"[+-]?\d+")

(re-matches optional-sign-integer "+123")


;Define un regex llamado word-then-digits que acepte una o más letras
;minúsculas seguidas de uno o más dígitos.

(def word-then-digits #"[a-z]+\d+")

(re-matches word-then-digits "abc123")


;Define un regex llamado tag-number que acepte un #, luego una o más
;letras minúsculas, luego -, luego exactamente dos dígitos.

(def tag-number #"[#][a-z]+[-]\d{2}")

(re-matches tag-number "#x-99")



;Define un regex llamado user-code que acepte: una o más letras
; minúsculas luego @, luego exactamente 3 dígitos

(def user-code #"[a-z]+[@]\d{3}")

(re-matches user-code "ana@123")

;Define un regex llamado money-code que acepte: un signo $ literal
; luego una o más letras mayúsculas luego -, luego exactamente 2 dígitos

(def money-code #"[$][A-Z]+-\d{2}")

(re-matches money-code "$ABC-12")

;Define un regex llamado paren-number que acepte: un paréntesis
; izquierdo ( , luego exactamente 3 dígitos luego un paréntesis derecho )

(def paren-number #"[\(]\d{3}[\)]")

(re-matches paren-number "(123)")

;Define un regex llamado scheme-radix que acepte enteros con prefijo Scheme:
;#b = binario, solo 0 y 1
;#o = octal, solo 0 a 7
;#d = decimal, solo 0 a 9
;#x = hexadecimal, 0 a 9, a-f, A-F

(def scheme-radix #"#b[01]+|#o[0-7]+|#d[0-9]+|#x[0-9 a-f A-F]")

(re-matches scheme-radix "#b101")


;----------------------------------------------------------
;Write a function in Clojure called how-many-div-4 that receives as
; input a sequence of integers and returns how many of these numbers
; are exactly divisible by 4.
(defn how-many-div-4
      [s]
      (count (filter #(zero? (mod % 4)) s)))

(how-many-div-4 [4 8 10 12 15]) ; => 3


(defn weirdo [a b]
      (apply +
             (loop [i a
                    r '()]
                   (if (> i b)
                     r
                     (recur (inc i)
                            (cons (if (zero? (rem i 2)) i 0)
                                  r))))))





