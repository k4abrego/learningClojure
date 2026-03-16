(ns more-repetitions
(:require [clojure.test :refer [deftest is run-tests]]))

;1
(defn expand
  [s]
  (mapcat repeat
       (range 1 (inc (count s)))
       s))

(expand '(a b c d)) ;=> (a b b c c c d d d d)

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

;----------------------------------------------------------
;2
;Recursive
;(defn insert
;  [n s]
;  (cond
;    (empty? s)  (list n)
;    (< n (first s)) (cons n s)
;    :else           (cons (first s)
;                          (insert n (rest s)))))

;loop
;(defn insert
;  [n s]
;  (loop [s s
;    result []]
;    (if (or (empty? s) (<= n (first s)))
;      (concat result [n] s) ;first case
;      (recur (rest s)
;             (conj result (first s))))))

;sequence API
(defn insert
  [n s]
  (let [split-result (split-with #(< % n) s)]
    (concat (first split-result)
            [n]
            (second split-result))))

(insert 5 '(1 3 6 7 9 16)) ;=> (1 3 5 6 7 9 16)
(insert 5 ()) ;=> (5)
(insert 4 '(-3 1 2 6 8)) ;=> (-3 1 2 4 6 8)

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

;----------------------------------------------------------
;3.
;recursion solution
;(defn insertion-sort ;O(N^2)
;  [s]
;  (if (empty? s)
;    ()
;    (insert (first s) ;O(N)
;            (insertion-sort (rest s)))))

;sequence API solution
(defn insertion-sort
  [s]
  (reduce (fn [accum x] (insert x accum))
          ()
          s))

(insertion-sort '(8 3 5 1)) ;=> (1 3 5 8)
(insertion-sort '(4 3 6 8 3 0 9 1 7)) ;=> (0 1 3 3 4 6 7 8 9)

(deftest test-insertion-sort
  (is (= () (insertion-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9)
         (insertion-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (insertion-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (insertion-sort '(5 5 5 1 5 5 5)))))

;----------------------------------------------------------
;4.
(defn rotate-left [n s]
      (let [len (count s)]
           (if (zero? len)
             '()
             (let [k (mod n len)]
                  (concat (drop k s) (take k s))))))

(rotate-left 3 '(a b c d e f g)) ;=> (d e f g a b c)
(rotate-left 2 '(1 2 3 4 5)) ;=> (3 4 5 1 2)

(deftest test-rotate-left
         (is (= () (rotate-left 5 ())))
         (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
         (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
         (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
         (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
         (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
         (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
         (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
         (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
         (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
         (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
         (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))
;----------------------------------------------------------

;5.
;; recursive sol
;(defn binary-aux
;  [n]
;  (if (zero? n)
;    ()
;    (cons (rem n 2)
;          (binary-aux (quot n 2)))))
;
;(defn binary
;      [n]
;      (reverse (binary-aux n)))

;loop/ recur solution
;(defn binary
;      [n]
;      (loop [n n
;             result ()]
;            (if (zero? n)
;              result
;              (recur (quot n 2)
;                     (cons (rem n 2)
;                           result)))))

;sequence API
(defn binary
      [n]
      (second
        (first
        (drop-while
          (fn [param]
              (let [n (first param)]
                   (not= n 0)))

       (iterate (fn [param]
                    (let [n (first param)
                          result (second param)]
                         [(quot n 2)
                          (cons (rem n 2) result)]))
                [n ()])))))

(binary 11) ;=> (1 1 0 1)


(deftest test-binary
         (is (= () (binary 0)))
         (is (= '(1 1 1 1 0) (binary 30)))
         (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

;----------------------------------------------------------
;6,
;recur solution
(defn prime-factors
      [n]
      (loop [n n
            result []
            divisor 2]
            (cond
              (= n 1)
              (concat result ())
              (zero? (rem n divisor))
              (recur (quot n divisor)
                     (conj result divisor)
                     divisor)
            :else
              (recur n
                     result
                     (inc divisor)))))
(prime-factors 666) ;=> (2 3 3 37)

(deftest test-prime-factors
         (is (= () (prime-factors 1)))
         (is (= '(2 3) (prime-factors 6)))
         (is (= '(2 2 2 2 2 3) (prime-factors 96)))
         (is (= '(97) (prime-factors 97)))
         (is (= '(2 3 3 37) (prime-factors 666))))

;----------------------------------------------------------
;7
(defn gcd [a b]
      (if (= b 0)
        a
        (recur b (mod a b))))
(gcd 20 16) ;=> 4

(deftest test-gcd
         (is (= 1 (gcd 13 7919)))
         (is (= 4 (gcd 20 16)))
         (is (= 6 (gcd 54 24)))
         (is (= 7 (gcd 6307 1995)))
         (is (= 12 (gcd 48 180)))
         (is (= 14 (gcd 42 56))))

;----------------------------------------------------------
;8.
(defn insert-everywhere [x s]
      (if (empty? s)
        (list (list x))
        (let [rest-insertions (insert-everywhere x (rest s))]
             (cons
               (cons x s)
               (map #(cons (first s) %) rest-insertions)))))

(insert-everywhere 1 '(* * * *))

(deftest test-insert-everywhere
         (is (= '((1)) (insert-everywhere 1 ())))
         (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
         (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
                (insert-everywhere 1 '(a b c))))
         (is (= '((1 a b c d e)
                  (a 1 b c d e)
                  (a b 1 c d e)
                  (a b c 1 d e)
                  (a b c d 1 e)
                  (a b c d e 1))
                (insert-everywhere 1 '(a b c d e))))
         (is (= '((x 1 2 3 4 5 6 7 8 9 10)
                  (1 x 2 3 4 5 6 7 8 9 10)
                  (1 2 x 3 4 5 6 7 8 9 10)
                  (1 2 3 x 4 5 6 7 8 9 10)
                  (1 2 3 4 x 5 6 7 8 9 10)
                  (1 2 3 4 5 x 6 7 8 9 10)
                  (1 2 3 4 5 6 x 7 8 9 10)
                  (1 2 3 4 5 6 7 x 8 9 10)
                  (1 2 3 4 5 6 7 8 x 9 10)
                  (1 2 3 4 5 6 7 8 9 x 10)
                  (1 2 3 4 5 6 7 8 9 10 x))
                (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

;----------------------------------------------------------
;9.
(defn contains-all-digits? [n]
      (let [digits (set (map #(Character/getNumericValue %)
                             (str (if (neg? n) (- n) n))))]
           (= (count digits) 10)))

(contains-all-digits? 1023456789) ;=> true
(contains-all-digits? 102345) ;=> false

(deftest test-contains-all-digits?
         (is (contains-all-digits? 1023456789))
         (is (contains-all-digits? 5897230146))
         (is (contains-all-digits? 10123485679))
         (is (contains-all-digits?
               1223334444555566666677777778888888889999999990))
         (is (not (contains-all-digits? 1236)))
         (is (not (contains-all-digits? 1112223334455)))
         (is (not (contains-all-digits? -587230462413578)))
         (is (not (contains-all-digits?
                    -122333444455556666667777777888888888999999999))))

;----------------------------------------------------------
;10.
(defn pack
      [s]
      (partition-by identity s))

(pack '(1 1 2 2 3 3 4)) ;=> ((1 1) (2 2) (3 3) (4))

(deftest test-pack
         (is (= () (pack ())))
         (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
                (pack '(a a a a b c c a a d e e e e))))
         (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
         (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

;----------------------------------------------------------
;11.
(defn compress [s]
      (map first (partition-by identity s)))

(compress '(a a a a b c c c c d d)) ;=> (a b c d)

(deftest test-compress
         (is (= () (compress ())))
         (is (= '(a b c d) (compress '(a b c d))))
         (is (= '(a b c a d e)
                (compress '(a a a a b c c a a d e e e e))))
         (is (= '(a) (compress '(a a a a a a a a a a)))))
;----------------------------------------------------------
;12.
(defn encode [s]
      (map (fn [grp] [(count grp) (first grp)])
           (partition-by identity s)))

(encode '(1 2 3 4)) ;=> ([1 1] [1 2] [1 3] [1 4])

(deftest test-encode
         (is (= () (encode ())))
         (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
                (encode '(a a a a b c c a a d e e e e))))
         (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
                (encode '(1 2 3 4 5))))
         (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))
;----------------------------------------------------------
;13.
(defn encode-modified [s]
      (map (fn [g]
               (let [cnt (count g)
                     elem (first g)]
                    (if (= cnt 1)
                      elem
                      [cnt elem])))
           (partition-by identity s)))

(encode-modified '(1 2 2 2 3 4 4 5)) ;=> (1 [3 2] 3 [2 4] 5)

(deftest test-encode-modified
         (is (= () (encode-modified ())))
         (is (= '([4 a] b [2 c] [2 a] d [4 e])
                (encode-modified '(a a a a b c c a a d e e e e))))
         (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
         (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

;----------------------------------------------------------
;14.
(defn decode [s]
      (mapcat (fn [x]
                  (if (vector? x)
                    (repeat (first x) (second x))
                    [x]))
              s))
(decode '(1 [3 2] 3 [2 4] 5)) ;=> (1 2 2 2 3 4 4 5)

(deftest test-decode
         (is (= () (decode ())))
         (is (= '(a a a a b c c a a d e e e e)
                (decode '([4 a] b [2 c] [2 a] d [4 e]))))
         (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
         (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)