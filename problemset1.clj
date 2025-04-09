;; Problem Set 1
;; Yvanna Cardenas
;;
;;

;; Problem 1
(defn abs [x] (Math/sqrt x))

;; Problem 2
;; 2.1
;; (defn take-square
;;   (* x x)) ;; Wrong because it doesn't have a parameter.

;; Corrected version
(defn take-square [x] (* x x))

;; 2.2
;; (defn sum-of-squares [(take-square x) (take-square y)]
;;   (+ (take-square x) (take-square y))) ;; Wrong because function calls cannot be the parameters. Parameters should be the arguments x and y

;; Corrected version
(defn sum-of-squares [x y]
  (+ (take-square x) (take-square y)))

;; Problem 3
(def exp-13-1 (- 15 2))
(def exp-13-2 (* 13 1))
(def exp-13-3 (/ 26 2))
(def exp-13-4 (+ 10 3))

;; Problem 4
(defn third [l] (first (rest (rest l))))

;; Problem 5
(defn chain [f g] (fn [x] (f (g x))))

;; Problem 6 
(defn last-two [l]
  (if (= (count l) 3)
    (rest l)
    (last-two (rest l))))

;; Problem 7
(defn remove-second [l] (cons
                          (first l)
                          (rest (rest l))))

;; Problem 8
(defn add-to-end [l x] 
  (if (empty? l)
    (list x)
    (cons (first l) (add-to-end (rest l) x))))

;; Problem 9 
(defn reverse [l]
  (if (empty? l)
    '()
    (cons (last l) (reverse (butlast l)))))

;; Problem 10
(defn reverse-nested [l]
  (if (empty? l)
    '()
    (cons (reverse (first l)) (reverse-nested (rest l)))))

;; Problem 11 given n, print (1, 2, 3, ..., n)
(defn count-to-n [n]
  (if (= n 0)
    '()
    (concat (count-to-n (- n 1)) (list n))))

;; Problem 12
(defn count-to-1 [n]
  (if (= n 0)
    '()
    (cons n (count-to-1 (- n 1)))))

;; Problem 13
(defn get-max [l]
  (if (empty? l)
    (first l)
    (let [temp (get-max (rest l))]
      (if (> (first l) temp)
        (first l)
        temp))))

;; Problem 14
(defn greater-than-seven? [l] 
(map (fn [x] (if (> x 7) true false) ) l))

;; Problem 15

;; Problem 16

;; Problem 17