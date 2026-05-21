(ns english_grammar
    (:require [clojure.test :refer [deftest is run-tests]])
    (:require [instaparse.core :refer [parser]])
    (:import (instaparse.gll Failure)))

(defn fails? [r] (instance? Failure r))
(defn succeeds? [r] (not (fails? r)))

(def simple-english_grammar ;(noun-phrase | epsilon)
  (parser "
  sentence = noun-phrase verb-phrase
  noun-phrase = article noun
  verb-phrase = verb [ noun-phrase ]
  article = spaces ('the' | 'a') spaces
  noun = spaces ('man' | 'woman' | 'ball' | 'table') spaces
  verb = spaces ('hit' | 'took' | 'saw' | 'liked') spaces
  spaces = #'\\s*'

  "))


(succeeds? (simple-english_grammar "a man hit a ball")) ;true
(succeeds? (simple-english_grammar "a man hit a girl")) ;false