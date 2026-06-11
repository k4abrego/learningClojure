(ns highlighter
  (:require [clojure.string :as str]))

(def regex #"(?x)
      ( -?\d+[.]\d*(?:E-?\d+)? | -?[.]\d+(?:E-?\d+)? | -?\d+ ) # Group 1: numbers
    | ( \"[^\"]*\" )                                           # Group 2: strings
    | ( REM[^\n]* )                                            # Group 3: comments
    | ( [A-Z]{1,2}\d?\$? )                                     # Group 4: variables
    | ( PRINT|GOTO|IF|THEN|FOR|TO|STEP|NEXT|END )              # Group 5: reserved words
    | ( ABS|SIN|COS|TAN|INT|TAB )                              # Group 6: predefined functions
    | ( <=|>=|<>|[()+\-*/^=<>:;,] )                            # Group 7: punctuation
    | ( \s )                                                   # Group 8: whitespace
    | ( . )                                                    # Group 9: bad token
")

(def categories
  [nil :number :string :comment :variable :reserved-word
   :function :punctuation :whitespace :bad-token])

(defn capturing-group-index ;returns the index of the matching regex"
  [v]
  (inc (count (take-while nil? (rest v)))))

(defn lexical-analysis
  [file-content]
  (let [matches (re-seq regex file-content)]
    (map (fn [match]
           [(match 0) (categories (capturing-group-index match))])
         matches)))

(defn escape  ;replaced in the output document by their corresponding escape sequences
  [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))
;

(defn token->html ;converts a tokenn and its category into a span
  [[token category]]
  (if (= category :whitespace)
    token
    (format "<span class=\"%s\">%s</span>"
            (name category)
            (escape token))))

(defn htmlize ;the complete token sequence into highlighted html
  [tokens]
  (apply str (map token->html tokens)))

(defn basic->html
  [file-name]
  (let [file-content (slurp file-name)
        html-content (htmlize (lexical-analysis file-content))
        template (slurp "Project/sinewave.html")
        output-name (str/replace file-name #"\.bas$" ".html")]
    (spit output-name
          (str/replace template "flop" html-content))))

; (lexical-analysis (slurp "sinewave.bas"))
; (basic->html "sinewave.bas")

(basic->html "Project/sinewave.bas")