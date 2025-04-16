;; Problem 1: Write a procedure, called sequence-to-power, that takes a sequence (represented as a list) x,
;; and a positive integer n, and returns the sequence xn. For example, given the sequence (list ’a ’b) and the
;; number 3, the procedure should return (list ’a ’b ’a ’b ’a ’b).



;; Problem 2: Define L as a language containing a single sequence, L= {[b]}. (Note: the notation [b] means
;; the same thing as (list ’b), which is what we have been using in class). Write a procedure in-L? that takes a
;; sequence (represented as a list), and decides if it is a member of the language L∗. That is, given a sequence
;; x, the procedure should return true if and only if x is a member of L∗, and false otherwise.



;; Problem 3: Define the language bnan as follows: it is the set of strings consisting of k b’s followed by k
;; a’s, for all k > 0, i.e. bnan = {[ba], [bbaa], [bbbaaa], ...}. Write a procedure generate-bn-an, which takes a
;; single argument k (which you can assume to be an integer greater than 0). It should return the k’th element
;; of the language bnan. For example, given the number 3, it should return (list ’b ’b ’b ’a ’a ’a).
;; Hint: Use the function add-to-end that you wrote in Problem Set 1.



;; Problem 4: Write a procedure remove-last-element, which takes a single list l as its argument. The
;; procedure should return the list l with its last element removed. For example, given the input (list ’a ’b ’b),
;; the procedure should return (list ’a ’b).
;; Hint: Use the procedure reverse that you wrote in Problem Set 1.



;; Problem 5: Writeaprocedurerecognize-bn-an, thattakesaninputstring strasitsargument, andreturns
;; true if str is a member of bnan and false otherwise. For example, the procedure should return false on the
;; input (list ’b ’b ’a ’a ’b).
;; Hint: Use the procedures reverse and remove-last-element from the last problem.




;; Problem 6: Write a procedure recognize-palindrome, that takes two arguments: a string str and a list L.
;; When called with (recognize-palindrome str ’()), it returns true if str is a palindrome and false otherwise.
;; For example, the procedure should return true on the input (list ’c ’a ’b ’b ’a ’c). It should return false on
;; the input (list ’c ’c ’a ’b ’b ’a ’c ’d).



;; Problem 7: Define L as a language containing a single string. For example, we may have L= {[a]} or
;; L= {[b]}. You can assume that L is presented to you as a single string, for example, one possible value for
;; L is (list ’a). (Note: In general, you should assume that languages are presented to you as a list of lists, i.e.
;; a list of strings. This problem is a special case, as L is assumed to contain only a single string.)
;; Define A to be any language consisting of a finite set of sequences. You can assume A is represented as
;; a list of lists. For example, one possible value for A is (list (list ’a ’b) (list ’b ’b)), which would represent
;; the language {[ab], [bb]}.
;; Write a procedure concat-L-A that takes two arguments, L and A, as defined above. It should return
;; concat(L, A), the concatenation of L and A. The return value should be represented as a list of lists (i.e.,
;; a list of strings).



;; Problem 8: concat(B, A).
;; Let A and B be languages. Find an example of languages A and B such that concat(A, B) =
;; Problem 9: does not equal concat(B, A)
;; Let A and B be languages. Find an example of languages A and B such that concat(A, B)



;; Problem 10: Find an example of a language L such that L= L2, i.e. L= concat(L, L).



;; Problem 11: Write a procedure matches-a+b*c? that takes a single input str as its argument, and
;; returns true if the string is in the language a+b*c?, and false otherwise. Here, a+b*c? means one or more
;; ’a’ characters, followed by zero or more ’b’ characters, followed by zero or one ’c’ character.



;; Problem 12: Write a procedure kleene-star that takes a language L (represented as a list of strings) and
;; a non-negative integer n, and returns the Kleene star of L up to n repetitions. The Kleene star of a language
;; L, denoted as L*, is the set of all strings that can be formed by concatenating zero or more strings from L.
;; For example, if L = (list "a" "ab"), then:
;; • (kleene-star L 0) should return (list "")
;; • (kleene-star L 1) should return (list "" "a" "ab")
;; • (kleene-star L 2) should return (list "" "a" "ab" "aa" "aab" "aba" "abab")
;; To solve this problem, you will need to generate all possible concatenations of strings from L of length
;; up to n.