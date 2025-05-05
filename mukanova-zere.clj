;; Problem Set 1
;; Yvanna Cardenas, Jeong Lee, Zere Mukanova
;; Contributions: 
;; We all first solved all the questions before the group meeting 
;; and then met up, discussed and solved difficult questions together.

;; Problem 1
(defn sequence-to-power [x n]
  (if (= n 1)
    x
    (concat x (sequence-to-power x (- n 1)))))


;; Problem 2
(defn prefix? [prefix lst]
  (if (= (first lst) (first prefix))
    true
    false))

(defn in-L? [x]
  (if (= (count x) 0)
    true
    (if (prefix? '(b) x)
      (in-L? (rest x))
      false)))

(in-L? '())


;; Problem 3
; Helper function from Problem Set 1
(defn add-to-end [l x]
  (if (empty? l)
    (list x)
    (cons (first l) (add-to-end (rest l) x))))


(defn generate-bn-an [k]
  (if (= 1 k)
    (list 'b 'a)
    (add-to-end (cons 'b (generate-bn-an (- k 1))) 'a)))


;; Problem 4
(defn last-list [l]
  (if (empty? (rest l))
    (first l)
    (last-list (rest l))))

;; helper function to return the rest of the list except for the last element
(defn except-last [l]
  ;; if rest of the list is empty, then return empty list (bc it's the last element)
  (if (empty? (rest l))
    ();
    ;; else: if the rest of the list is not empty, cons a list with the first element 
    ;; and recursively apply except-last function to the rest
    (cons (first l) (except-last (rest l)))))

(defn reverse [l]
  (if (empty? l)
    '()
    (cons (last-list l) (reverse (except-last l)))))

(defn remove-last-element [l]
  (if (empty? l)
    '()
    (reverse (rest (reverse l)))))



;; Problem 5
(defn recognize-bn-an [str]
  (if (empty? str)
    true
    (if (= (first str) "b")
      (if (= (first (reverse str)) "a")
        (recognize-bn-an (rest (remove-last-element str)))
        false)
      false)))



;; Problem 6
(defn recognize-palindrome [str L]
  (if (= str (reverse str))
    true
    false))



;; Problem 7
(defn join-two [l1 l2]
  (if (empty? l1)
    l2
    (cons (first l1) (join-two (rest l1) l2))))

(defn concat-L-A [L A]
  (if (empty? A)
    L
    (map (fn [x] (join-two L x)) A)))




;; Problem 8: concat(B, A).
;A = {[a]}
;B = {[a]}


;; Problem 9: does not equal concat(B, A)
;A = {[a]}
;B = {[b]}

;; Problem 10: Find an example of a language L such that L= L2, i.e. L= concat(L, L).
;L = {[]}


;; Problem 11
(defn skip-as [s]
  (if (empty? s)
    s
    (if (= \a (first s))
      (skip-as (rest s))
      s)))

;; Helper Function 2: Drop every leading \b
(defn skip-bs [s]
  (if (empty? s)
    s
    (if (= \b (first s))
      (skip-bs (rest s))
      s)))

;; Helper Function 3: Check that what's left is zero or exactly one \c
(defn check-c [s]
  (if (empty? s)
    true
    (if (= \c (first s))
      (empty? (rest s))
      false)))

(defn matches-a+b*c? [str]
  ;; If the string is empty return false   
  (if (empty? str)
    false
    ;; If the first character is \a, skip all leading \a's and check the rest
    (if (= \a (first str))
      ;; Check if the rest of the string has zero or more \b's and then check for \c   
      (check-c (skip-bs (skip-as str)))
      false)))




;; Problem 12

;; rewrite join-two-string and concat-L-A so that they work for strings instead of lists
(defn join-two-string [s1 s2]
  (str s1 s2)
)

(defn concat-L-A-string [L A]
  (apply concat
         (map (fn [s1]
                (map (fn [s2]
                       (join-two-string s1 s2)) A)) L)
  )
)

;; Helper function: Gets new part of L, basically length n part ex) "aa" "aab" "aba" "abab" of L 2
(defn kleene-star-helper [L n]
  (cond
    (= n 0) '("")
    (= n 1) L
    :else
      (concat-L-A-string L (kleene-star-helper L (- n 1)))
  )
)

;; recursively adds new part of the strings to the existing parts from n-1
;; l is a list of strings like L = (list "a" "ab")
(defn kleene-star [L n]
  (if (= n 0)
    '("")
    (concat (kleene-star L (- n 1)) (kleene-star-helper L n))
  )
)