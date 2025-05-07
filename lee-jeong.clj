;; Problem Set 3
;; Yvanna Cardenas, Jeong Lee, Zere Mukanova
;; Contributions: 
;; We all first solved all the questions before the group meeting 
;; and then met up, discussed and solved difficult questions together.


;#1
;; given helper functions and variables

(def vocabulary '(call me ishmael))

(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))

(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))

(def thetas (list theta1 theta2))

(def theta-prior (list (/ 1 2) (/ 1 2)))

;; finds probability of outcome in outcomes
(defn score-categorical [outcome outcomes params]
  (if (empty? params)
    (/ 1 0)
    (if (= outcome (first outcomes))
      (first params)
      (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
  (if (empty? lst)
    base
    (f (first lst)
      (list-foldr f base (rest lst)))))

;; apply log
(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

;; calculates log probability of BOW
(defn score-BOW-sentence [sen probabilities]
  (list-foldr
    (fn [word rest-score]
      (+ (log2 (score-categorical word vocabulary probabilities))
        rest-score))
    0
  sen))

;; calculates log probability of corpus (it's a log probabillity becuase it uses score-BOW-sentence)
(defn score-corpus [corpus probabilities]
  (list-foldr
    (fn [sen rst]
      (+ (score-BOW-sentence sen probabilities) rst))
    0
    corpus))

;; wanna compute log(a + b + c) but only know log a, log b, log c
;; log-vals: a list of log probabilities
(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
      (log2
        (apply +
          (map (fn [z] (Math/pow 2 z))
            (map (fn [x] (- x mx)) log-vals)))))))

;; initial corpus
(def my-corpus '((call me)
                 (call ishmael)))

;; log Pr(C,θ) = log (Pr(C|θ) Pr(θ))
;; log Pr(C,θ) = log (Pr(C|θ)) + log (Pr(θ))

;; log (Pr(C|θ)) = score-corpus corpus theta
;; log (Pr(θ)) = log2 (score-categorical theta thetas theta-probs)

(defn theta-corpus-joint [theta corpus theta-probs]
  (let [log-prob-theta (log2 (score-categorical theta thetas theta-probs))]
    (+ (score-corpus corpus theta) log-prob-theta))
)

;; -6 + (-1) = -7
;; should return -7
(theta-corpus-joint theta1 my-corpus theta-prior)


;#2
;; Pr(C= corpus,θ= theta)
;; Hint: Use the logsumexp function defined above.
;; log(Sigma θ∈Θ Pr(C= corpus,Θ = θ))

(defn compute-marginal [corpus theta-probs]
  (logsumexp (map (fn [theta] (theta-corpus-joint theta corpus theta-probs)) thetas)
  )
)

(compute-marginal my-corpus theta-prior)


;#3
;; wanna get log (Pr(Θ = θ|C= corpus))
;; log Pr(θ|C) = log Pr(C,θ) - log (Sigma θ∈Θ Pr(C= corpus,Θ = θ))
;; from problem 1, we know log Pr(C,θ)
;; from problem 2, we know log (Sigma θ∈Θ Pr(C= corpus,Θ = θ))

(defn compute-conditional-prob [theta corpus theta-probs]
  (- (theta-corpus-joint theta corpus theta-probs) (compute-marginal corpus theta-probs))
)


;#4
(defn compute-conditional-dist [corpus theta-probs]
  (let [result1 (theta-corpus-joint theta1 corpus theta-probs)
        result2 (theta-corpus-joint theta2 corpus theta-probs)
        marginalCorpus (logsumexp (list result1 result2))]
    (list (- result1 marginalCorpus)
          (- result2 marginalCorpus))))

;#5
(compute-conditional-dist my-corpus theta-prior)
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
    


;#6
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
; so the probability of accuracy of the new corpus is higher than the old one because it has been trained



; Given helper functions from Problem 7:
;; normalize
(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

;; coin flip
(defn flip [weight]
  (if (< (rand 1) weight)
    true false))

;; picks one from outcomes
(defn sample-categorical [outcomes params] (if (flip (first params))
                                             (first outcomes) (sample-categorical (rest outcomes)
                                                                                  (normalize (rest params)))))

;; repeats function call n times                                                                                
(defn repeat [f n] (if (= n 0)
                     '()
                     (cons (f) (repeat f (- n 1)))))

;; samples a sentence from the bag of words model of length len, given the parameters theta
(defn sample-BOW-sentence [len probabilities] (if (= len 0)
                                                '()
                                                (cons (sample-categorical vocabulary probabilities)
                                                      (sample-BOW-sentence (- len 1) probabilities))))


;#7
(defn sample-BOW-corpus [theta sent-len corpus-len]
  (repeat (fn [] (sample-BOW-sentence sent-len theta)) corpus-len)
)


;#8
;; returns a list with two elements: 
;; a value of θ sampled from the distribution defined by theta-probs
;; and a corpus sampled from the bag of words model given the sampled θ. 

;; corpus-len = the number of sentences in the corpus
;; sent-len = number of words in each sentence

;; a value of θ sampled from the distribution defined by theta-probs = (sample-categorical thetas theta-probs)
;; a corpus sampled from the bag of words model given the sampled θ = (sample-BOW-corpus theta sent-len corpus-len)


(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (let [theta (sample-categorical thetas theta-probs)]
    (let [corpus (sample-BOW-corpus theta sent-len corpus-len)]
      (list theta corpus)
    )
  )
)


;#9
;; Define a procedure estimate-corpus-marginal, the prcedure should return
;; an estimate of the marginal likelihood of the target corpus, using the formula
;; defined in Equestion 3
(defn get-theta [theta-corpus]
  (first theta-corpus))

(defn get-corpus [theta-corpus]
  (first (rest theta-corpus)))

(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
  (repeatedly sample-size #(sample-theta-corpus sent-len corpus-len theta-probs)))

(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (list theta1 '((call me) (call ishmael))))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
  (let [samples (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)
        corpora (map get-corpus samples)
        match-count (count (filter #(= % corpus) corpora))]
    (/ match-count sample-size)))

(estimate-corpus-marginal my-corpus 10 2 2 theta-prior)

;; #10
(estimate-corpus-marginal my-corpus 50 2 2 theta-prior) 
;; Since we were calling the function with a sample size of 50, the output
;; would fluctuate from run to run. In some runs, the estimated probability
;; of the corpus was 0.0, while in others it was 0.08. This variability is 
;; is due to the smaller number of samples which leads to high variance. 

(estimate-corpus-marginal my-corpus 2000 2 2 theta-prior)
;; The estimated marginal likelihood stabalized with the output of around 0.01.
;; This is not surprising since it is expected for the estimate to become
;; more stable as the sample size increases. 

;; COmpare to the exactmarginal likelihood computed in Problem 2?
;; In problem 2 we computed the exact log probability value which was about -6.4. 
;; If we were to exponentiate the value we would get a value of around 0.009 or 0.01.
;; In this case, when using a sample size of 2000 while estimating, the result stabalize
;; at around 0.009 or 0.01 which is very close to what we found in problem 2. This 
;; confirms that the sampling based estimation converges correctly as the size of the 
;; sample size increases. 


;; #11
(defn rejection-sampler [theta observed-corpus sample-size sent-len corpus-len theta-probs]
  (let [samples (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)
        accepted (filter #(= (get-corpus %) observed-corpus) samples)
        accepted-thetas (map get-theta accepted)
        counts (get-counts thetas accepted-thetas) ; returns list of counts for each theta
        theta-index (.indexOf thetas theta)
        theta-count (nth counts theta-index)
        total (count accepted)]
    (if (zero? total)
      0
      (/ theta-count total))))

;; #12
(rejection-sampler theta1 my-corpus 100 2 2 theta-prior)
;; After calling the rejection-sample function with a sample size of 100, 
;; a number otimes, I noticed that the results fluctuated significantly. 
;; The sample size needs to be to be increased to 1000 or 2000 to start 
;; getting a stable estimate of the conditional probability. It takes so
;; many samples to get a stable estimate because rejection sampling discards
;; all samples that do not match the observed corpus. This means that these
;; matches are rare, and even more when we do not have a large sample size.
