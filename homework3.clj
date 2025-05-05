;1


;2




;3



; Givens: 
(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2) (/ 1 4) (/ 1 4))) (def theta2 (list (/ 1 4) (/ 1 2) (/ 1 4)))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))

; Given helper functions from Problem 1:
(defn score-categorical [outcome outcomes params] (if (empty? params)
                                                    (/ 1 0)
                                                    (if (= outcome (first outcomes))
                                                      (first params)
                                                      (score-categorical outcome (rest outcomes) (rest params)))))
1
(defn list-foldr [f base lst] (if (empty? lst)
                                base
                                (f (first lst)
                                   (list-foldr f base (rest lst)))))
(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))
(defn score-BOW-sentence [sen probabilities] (list-foldr
                                              (fn [word rest-score]
                                                (+ (log2 (score-categorical word vocabulary probabilities))
                                                   rest-score)) 0
                                              sen))
(defn score-corpus [corpus probabilities] (list-foldr
                                           (fn [sen rst]
                                             (+ (score-BOW-sentence sen probabilities) rst))
                                           0 corpus))

(defn logsumexp [log-vals]
      (let [mx (apply max log-vals)]
        (+ mx (log2
               (apply +
                      (map (fn [z] (Math/pow 2 z))
                           (map (fn [x] (- x mx)) log-vals)))))))

; TEMPORARY AS REPLACEMENT FOR JEONG'S QUESTION 1 - NEED TO DELETE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defn theta-corpus-joint [theta corpus theta-probs]
  (let [corpus-score (score-corpus corpus theta) ;; log P(corpus | theta)
        index (if (= theta theta1) 0 1)           ;; find index of theta
        prior-prob (nth theta-probs index)       ;; P(theta)
        prior-score (log2 prior-prob)]           ;; log P(theta)
    (+ corpus-score prior-score)))                ;; log P(C, theta)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;4
(defn compute-conditional-dist [corpus theta-probs]
  (let [result1 (theta-corpus-joint theta1 corpus theta-probs)
        result2 (theta-corpus-joint theta2 corpus theta-probs)
        marginalCorpus (logsumexp (list result1 result2))]
    (list (- result1 marginalCorpus)
          (- result2 marginalCorpus))))

;5

; results before exponentiation: (-0.5849625007211561 -1.584962500721156)
; conditional distribution after exponentiation:
    (def p1 (Math/pow 2 -0.5849625007211561))
    (def p2 (Math/pow 2 -1.584962500721156))
    
    (println "P(theta1 | corpus):" p1)
    (println "P(theta2 | corpus):" p2)

; results after exponentiation: 
;  P(theta1 | corpus): 0.6666666666666667
;  P(theta2 | corpus): 0.33333333333333337

; Explanation: 
; The conditional distribution of the two thetas given the corpus indicates that theta1 is more 
; likely than theta2, because theta1 assigns a higher probability to the word 'call' that repeats the most
; in the corpus. So theta1 aligns with the word counts in the corpus more than theta2. 
    


;6
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs] 
  (let 
   [conditional-dist (compute-conditional-dist observed-corpus theta-probs) ;log probabilities of thetas
    corpusTheta1 (score-corpus new-corpus theta1) ;log P(corpus | theta1)
    corpusTheta2 (score-corpus new-corpus theta2) ;log P(corpus | theta2)
    theta1Result (+ (first conditional-dist) corpusTheta1) ;log P(theta1 | corpus) + log P(corpus | theta1)
    theta2Result (+ (last conditional-dist) corpusTheta2) ;log P(theta2 | corpus) + log P(corpus | theta2) 
    ] 
    (logsumexp (list theta1Result theta2Result)) ;log P(corpus | theta)
    )
)

; probability of the new corpus (in this case same as the old one) being generated after observing the old corpus
; results of calling (compute-posterior-predictive my-corpus my-corpus theta-prior): -6.2630344058337934
; 2^(-6.2630344058337934) = 0.0104 ->  around 1 percent probability of generating the same corpus again
; comparing to marginal likelihood from number 2, this value takes the updated beliefs learned from prior corpus into account

; Given helper functions from Problem 7:
(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))
(defn flip [weight]
  (if (< (rand 1) weight)
    true false))
(defn sample-categorical [outcomes params] (if (flip (first params))
                                             (first outcomes) (sample-categorical (rest outcomes)
                                                                                  (normalize (rest params)))))
(defn repeat [f n] (if (= n 0)
                     '()
                     (cons (f) (repeat f (- n 1)))))
(defn sample-BOW-sentence [len probabilities] (if (= len 0)
                                                '()
                                                (cons (sample-categorical vocabulary probabilities)
                                                      (sample-BOW-sentence (- len 1) probabilities))))
;7
(defn sample-BOW-corpus [theta sent-len corpus-len]
  (repeat (fn [] (sample-BOW-sentence sent-len theta)) corpus-len)
  )
    
