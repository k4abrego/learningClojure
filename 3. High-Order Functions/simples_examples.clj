(ns simples-examples)

(defn $map
      [fn s]
      (if (empty? s)
        ()
        (cons (fn (first s))
              ($map fn (rest s)))))

($map #(* 2 %) [4 1 2 3 0 -10]) ;=> (8 2 4 6 0 -20)

(defn add
      [number] ;add
      (fn [x] (+ x number))) ; 10 + 5

(def f (add 5))
(f 10); ;f = f [x]

((add 50) 1)

(defn h
      [a b c d e]
      (* a (+ b c (- d e))))

(h 2 3 4 5 6) ;=> 12
 ;^
(defn h-curry
      [a]
      (fn [b]
          (fn [c]
              (fn [d]
                  (fn [e]
                      (* a (+ b c (- d e))))))))

(((((h-curry 2) 3) 4) 5) 6) ;=> 12

(defn add-all
      [& params] ;cualquier cantidad de argumentos
      (if (empty? params)
        0
        (+ (first params) (apply add-all (rest params)))))


(add-all)
(add-all 1 2 3 4 5)

(defn composite
  [f g]
  (fn [x] (f (g x))))

(defn f1 [x] (* 3 x))
(defn f2 [x] (+ x 5) )
(def f3 (composite f1 f2)) ;its not the fn
(def f4 (composite f2 f1))
(def f5 (composite f3 f4))

(f1 1)
(f2 1)
(f3 1)
(f4 1)
(f5 1)






;----------------------------------------------------------
;4.
(defn rotate-left
      [n s]
      (if (empty? s)
        ()
        (let [len (count s)
              k (mod n len)
              parts (split-at k s)]
             (concat (second parts) (first parts)))))

;----------------------------------------------------------
;7.
(defn gcd
      [a b]
      (loop [a a
             b b]
            (if (zero? b)
              a
              (recur b (rem a b)))))

;----------------------------------------------------------
;8.
(defn insert-everywhere
      [x s]
      (map (fn [i]
               (concat (take i s)
                       (list x)
                       (drop i s)))
           (range (inc (count s)))))

;----------------------------------------------------------
;9.
(defn contains-all-digits?
      [n]
      (and (not (neg? n))
           (= (set "0123456789")
              (set (str n)))))

