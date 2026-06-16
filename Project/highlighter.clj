(ns highlighter
    (:require [clojure.string :as str]))

(def regex
  #"(?x)
      ( -?(?:\d+[.]\d*|[.]\d+|\d+)(?:E[+-]?\d+)? )           # numbers
    | ( \"[^\"]*\" )                                          # strings
    | ( REM[^\n]* )                                           # comments
    | ( PRINT|GOTO|IF|THEN|FOR|TO|STEP|NEXT|END )             # reserved words
    | ( ABS|SIN|COS|TAN|INT|TAB )                             # predefined functions
    | ( [A-Z] \w* )                                           # variables
    | ( <=|>=|<>|[()+\-*/^=<>:;,] )                           # punctuation
    | ( \s )                                                  # whitespace
    | ( . )                                                   # bad token
")

(def categories
  [nil :number :string :comment :reserved-word :function
   :variable :punctuation :whitespace :bad-token])

(defn capturing-group-index
      [v]
      (inc (count (take-while nil? (rest v)))))

(defn lexical-analysis
      [file-content]
      (let [matches (re-seq regex file-content)]
           (map (fn [match]
                    [(match 0) (categories (capturing-group-index match))])
                matches)))

(defn escape
      [s]
      (-> s
          (str/replace "&" "&amp;")
          (str/replace "<" "&lt;")
          (str/replace ">" "&gt;")))

(defn token->html
      [[token category]]
      (if (= category :whitespace)
        token
        (format "<span class=\"%s\">%s</span>"
                (name category)
                (escape token))))

(defn htmlize
      [tokens]
      (apply str (map token->html tokens)))

(defn html-document
      [contenido]
      (str "<!DOCTYPE html>\n"
           "<html>\n"
           "<head>\n"
           "  <meta charset=\"UTF-8\">\n"
           "  <title>BASIC Lexical Highlighter</title>\n"
           "  <link rel=\"stylesheet\" href=\"styles.css\">\n"
           "</head>\n"
           "<body>\n"
           "<pre>" contenido "</pre>\n"
           "</body>\n"
           "</html>\n"))

(defn basic->html
      [file-name]
      (let [file-content (slurp file-name)
            html-content (htmlize (lexical-analysis file-content))
            output-name (str/replace file-name #"\.bas$" ".html")]
           (spit output-name (html-document html-content))))

(load-file "Project/highlighter.clj")
;(highlighter/basic->html "Project/otro.bas")
(basic->html "Project/banner.bas")