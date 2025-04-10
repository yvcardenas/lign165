;; Problem Set 1
;; Yvanna Cardenas
;;
;;

;; Problem 1
(defn abs [x] (Math/sqrt x))
(abs 4)

;; Problem 2
;; 2.1
;; (defn take-square
;;   (* x x)) ;; Wrong because it doesn't have a parameter

;; Corrected version
(defn take-square [x] (* x x))
(take-square 2)

;; 2.2
;; (defn sum-of-squares [(take-square x) (take-square y)]
;;   (+ (take-square x) (take-square y))) ;; Wrong because it functions cannot be the parameters, thhe parameters should be the arguments x and y

;; Corrected version
(defn sum-of-squares [x y]
  (+ (take-square x) (take-square y)))

(sum-of-squares 2 3)

;; Problem 3
(def exp-13-1 (- 15 2))
(def exp-13-2 (* 13 1))
(def exp-13-3 (/ 26 2))
(def exp-13-4 (+ 10 3))
exp-13-1 
exp-13-2
exp-13-3
exp-13-4

;; Problem 4
(defn third [l] (first (rest (rest l))))
(third '(4 5 6))

;; Problem 5
(defn chain [f g] (fn [x] (f (g x))))

(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))
((chain sqrt abs) -36)

;; Problem 6 
(defn last-two [l]
  (if (= (count l) 3)
    (rest l)
    (last-two (rest l))))
(last-two '(4 5 6))

;; Problem 7
(defn remove-second [l] (cons
                          (first l)
                          (rest (rest l))))
(remove-second (list 3 1 4))

;; Problem 8
(defn add-to-end [l x] 
  (if (empty? l)
    (list x)
    (cons (first l) (add-to-end (rest l) x))))
(add-to-end(list 5 6 4) 0)

;; Problem 9 
(defn reverse [l]
  (if (empty? l)
    '()
    (cons (last l) (reverse (butlast l)))))
(reverse '(a b c))

;; Problem 10
(defn reverse-nested [l]
  (if (empty? l)
    '()
    (cons (reverse (first l)) (reverse-nested (rest l)))))
(reverse-nested '((a b c) (d e f)))

;; Problem 11 given n, print (1, 2, 3, ..., n)
(defn count-to-n [n]
  (if (= n 0)
    '()
    (concat (count-to-n (- n 1)) (list n))))
(count-to-n 4)

;; Problem 12
(defn count-to-1 [n]
  (if (= n 0)
    '()
    (cons n (count-to-1 (- n 1)))))
(count-to-1 4)

;; Problem 13
(defn get-max [l]
  )

;; Problem 14
(defn greater-than-seven? [l] 
(map (fn [x] (if (> x 7) true false) ) l))

;; Problem 15
(defn join-two [lst1 lst2]

  (if (empty? lst1)
    lst2
    (cons (first lst1) (join-two (rest lst1) lst2)))
)

(defn concat-three [x y z] (join-two (join-two x y) z))
(concat-three '(a b) '(b c) '(d e))

;; Problem 16

;; Problem 17
