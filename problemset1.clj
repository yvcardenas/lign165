;; Problem Set 1
;; Yvanna Cardenas
;;
;;

;; Problem 1
(defn abs [x] (Math/sqrt (* x x)))


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
  (map (fn [x] 
    (if (> x 7) true false)) l))


;; Problem 15
(defn join-two [l1 l2]

  (if (empty? l1)
    l2
    (cons (first l1) (join-two (rest l1) l2))))

(defn concat-three [x y z] (join-two (join-two x y) z))


;; Problem 16
(defn interleave [l1 l2]
  (cond
    ;; when both lists are empty list
    (and (empty? l1) (empty? l2)) '()
    ;; when l1 is empty list
    (empty? l1) l2
    ;; when l2 is empty list
    (empty? l2) l1
    ;; else (when none of them is empty)
    :else
      (cons (first l1)
        ;; list part of cons 
        (cons (first l2) 
          ;; list part of cons
          ;; this part is recursive
          (interleave (rest l1) (rest l2))
        )
      ) 
  )
)


;; Problem 17
(defn flatten [l]
  (if (empty? l)
    ;; then
    '()
    ;; else
    ;; (apply concat l)
    (let [first-list (first l)
          rest-list (rest l)]

          (if (list? first-list)
          ;; then
          (concat (flatten first-list) (flatten rest-list))
          ;; else
          (cons first-list (flatten rest-list))
          )
    )
  )
) 
;; 1. if empty list - return empty list
;; 2. else not empty list
;;   copy elements in the list until there is a list inside
;;   recursively check if there is a list inside
;;     if there is a list inside, copy elements in the list until 

;; 1. if list is empty
;;   return empty list
;; 2. else
;;   for each element
;;   (we need to divide the list to first and rest)
;;     check if an element(first) is a list
;;     - if it's a list: apply flatten to first element + apply flatten function to the rest
;;     - else(if it's not list): first element + apply flatten function to the rest


;; (1 2 (3 (4 5) 6) 7 8)
;; 1. (3 (4 5) 6) is a list, 1 2 7 8 is considered just regular elements
;; 2. In (3 (4 5) 6), (4 5) is a list

   