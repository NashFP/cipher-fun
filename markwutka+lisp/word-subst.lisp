(load "analysis.lisp")

;;; Returns the first upper-case letter in a word
(defun first-upper (word pos)
  (if (>= pos (length word)) nil
      (if (upper-case-p (aref word pos)) (aref word pos)
	  (first-upper word (1+ pos)))))

;;; Creates a pattern from a word, where the first unique character
;;; in the word is replaced by 'a', the next by 'b', then 'c'. Two
;;; words that have the same pattern of unique and repeated letters
;;; should have the same pattern (e.g. "book" and "look" both have
;;; the pattern "abbc"
(defun word-pattern (word)
  (labels ((word-pattern-iter (word next-sub)
	     (let ((upper (first-upper word 0)))
	       (if upper
		   (word-pattern-iter (substitute next-sub upper word)
				      (code-char (1+ (char-code next-sub))))
		   word))))
    (intern (word-pattern-iter word #\a))))

;;; Add each word to its corresponding pattern key in the pattern dict
(defun make-pattern-dict-iter (words dict)
  (if (null words) dict
      (let* ((word-sym (word-pattern (car words)))
	     (dict-entry (gethash word-sym dict '())))
	(setf (gethash word-sym dict) (cons (string-downcase (car words)) dict-entry))
	(make-pattern-dict-iter (cdr words) dict))))

(defun make-pattern-dict (words)
  (make-pattern-dict-iter (reverse words) (make-hash-table)))

;;; Get all the words with the same pattern from the dict
(defun get-words-with-same-pattern (word dict)
  (gethash (word-pattern word) dict '()))

;;; Split text into words with an eye towards cryptanalysis. Anything that isn't a letter is treated
;;; as a word break except for ', which is just ignored. It would probably be a good idea to add
;;; contractions to the word dictionary so these match (i.e. DON'T converts to DONT, so DONT ought
;;; to be in the dictionary
(defun split-text-iter (text pos in-word accum words)
  (if (>= pos (length text))
      (if in-word
	  (reverse (cons (list->string (reverse accum)) words))
	  (reverse words))
      (let ((ch (aref text pos)))
	(cond ((and (char>= ch #\A) (char<= ch #\Z))
	       (split-text-iter text (1+ pos) t (cons ch accum) words))
	      ((and (char>= ch #\a) (char<= ch #\z))
	       (split-text-iter text (1+ pos) t (cons (char-upcase ch) accum) words))
	      ((char= ch #\')
	       (split-text-iter text (1+ pos) in-word accum words))
	      (t
	       (if in-word
		   (split-text-iter text (1+ pos) nil '() (cons (list->string (reverse accum)) words))
		   (split-text-iter text (1+ pos) nil '() words)))
	      ))))

(defun split-text (text)
  (split-text-iter text 0 nil '() '()))

;;; Returns true of all the lowercase letters of two words match. When solving a cryptogram, we
;;; leave the encrypted letters upper-case, and when we try a particular substitution, we make the
;;; substitution lowercase. This way, we can compare a partially-solved word with all the words with
;;; the same pattern and eliminate pattern words that won't work because their letters don't match
;;; the already-solved letters in the word
(defun lc-matches (a b used-letters)
  (labels ((lc-matches-iter (pos)
	     (if (>= pos (length a)) t
		 (let ((a-ch (aref a pos))
		       (b-ch (aref b pos)))
		   (if (and (lower-case-p a-ch) (lower-case-p b-ch))
		       (if (char/= a-ch b-ch) nil
			   (lc-matches-iter (1+ pos)))
		       (if (and (upper-case-p a-ch) (lower-case-p b-ch)
				(find b-ch used-letters))
			   nil
			   (lc-matches-iter (1+ pos))))))))
    (lc-matches-iter 0)))

;;; Returns the number of characters that two words have in common
(defun num-common-characters (a b)
  (length (intersection (string->list a) (string->list b))))

;;; Find the word that has the most letters in common with the given word
(defun find-most-in-common-with (word words)
  (labels ((find-most-in-common-with-iter (words best-word best-count)
	     (if (null words) best-word
		 (if (equalp word (car words))
		     (find-most-in-common-with-iter (cdr words) best-word best-count)
		     (let ((common (num-common-characters word (car words))))
		       (if (>= common best-count)
			   (find-most-in-common-with-iter (cdr words) (car words) common)
			   (find-most-in-common-with-iter (cdr words) best-word best-count)))))))
    (find-most-in-common-with-iter words "" 0)))

;;; Find the two words in the list that have the most letters in common with each other
(defun find-most-in-common (words)
  (labels ((find-most-in-common-iter (curr-words best-word best-other best-count)
	     (if (null curr-words) (cons best-word best-other)
		 (let* ((best-common-word (find-most-in-common-with (car curr-words) words))
			(best-common-count (num-common-characters (car curr-words) best-common-word)))
		   (if (>= best-common-count best-count)
		       (find-most-in-common-iter (cdr curr-words)
						 (car curr-words)
						 best-common-word
						 best-common-count)
		       (find-most-in-common-iter (cdr curr-words)
						 best-word
						 best-other
						 best-count))))))
    (find-most-in-common-iter words "" "" 0)))

;;; Create a chain from start-word to the word that has the most letters in common
;;; with start-word, and then repeat the process with that word
(defun make-common-chain-from (start-word words)
  (labels ((make-common-chain-from-iter (start-word words accum)
	     (if (null words) (reverse accum)
		 (if (= 1 (length words)) (reverse (cons (car words) accum))
		     (let ((most-common (find-most-in-common-with start-word words)))
		       (make-common-chain-from-iter most-common
						    (remove most-common words :test #'equalp)
						    (cons most-common accum)))))))
    (make-common-chain-from-iter start-word words '())))

;;; Find the two words with the most letters in common, and then make a chain of words
;;; based on each word having the most letters in common with the previous word
(defun make-common-chain (words)
  (let* ((commoners (find-most-in-common words))
	 (head-word (car commoners))
	 (next-word (cdr commoners)))
    (cons head-word (cons next-word
			  (make-common-chain-from next-word
						  (remove head-word
							  (remove next-word words :test #'equalp)
							  :test #'equalp))))))

;;; Because we are solving from upper-case to lower-case, a word with all lower-case is solved
(defun is-word-solved (w)
  (equal w (string-downcase w)))

;;; Check to see if all the words in the text are solved
(defun is-text-solved (plaintext)
  (every #'is-word-solved plaintext))

;;; Try replacing ct-let with pt-let in word w
(defun solve-letter-in-word (w ct-let pt-let)
  (substitute pt-let ct-let w))

;;; Find all the substitutions necessary to solve word w as word solve-as
(defun get-new-substitutions (w solve-as)
  (labels ((iter (pos accum)
	     (if (>= pos (length w)) (reverse accum)
		 (let ((w-ch (aref w pos))
		       (s-ch (aref solve-as pos)))
		   (if (char= w-ch s-ch) (iter (1+ pos) accum)
		       (if (assoc w-ch accum)
			   (iter (1+ pos) accum)
			   (iter (1+ pos) (cons (cons (aref w pos) (aref solve-as pos)) accum))))))))
    (iter 0 '())))

;;; Apply a list of substitutions to a word
(defun apply-substitutions (word subs)
  (if (null subs) word
      (apply-substitutions (solve-letter-in-word word (caar subs) (cdar subs)) (cdr subs))))

;;; Apply a list of substitutions to a list of words
(defun apply-substitutions-to-word-list (words subs)
  (if (null words) nil
      (cons (apply-substitutions (car words) subs)
	    (apply-substitutions-to-word-list (cdr words) subs))))

;;; Remove all the words in a pattern that no longer can match against the partially-solved word
(defun filter-pattern (ct-word patterns used-letters)
  (remove-if-not (lambda (p) (lc-matches ct-word p used-letters)) patterns))

;;; For each word in the partially-solved text, remove all the words from their patterns that can
;;; no longer match the partially-solved word
(defun filter-patterns (plaintext ct-patterns used-letters)
  (if (null plaintext) nil
      (cons (filter-pattern (car plaintext) (car ct-patterns) used-letters)
	    (filter-patterns (cdr plaintext) (cdr ct-patterns) used-letters))))

;;; Try word as the solution for the first word in the order-of-solving list
(defun try-pattern-word (word order-of-solving plaintext ct-patterns)
  ;;; Find the substitutions needed to change the encrypted word to the proposed word
  ;;; This may just be a subset of the letters in the word since it may be partially-solved
  (let* ((subs (get-new-substitutions (car order-of-solving) word))
	 ;;; Adjust the words in order-of-solving to reflect the new substitutions
	 (new-order (apply-substitutions-to-word-list order-of-solving subs))
	 ;;; Update the partially-solved plaintext with the new substitutions
	 (new-pt (apply-substitutions-to-word-list plaintext subs))
	 ;;; Get the letters that have already been used
	 (used-letters (remove-if-not #'lower-case-p (apply #'concatenate 'string new-pt)))
	 ;;; For each word, remove any words in its pattern set that couldn't possibly
	 ;;; match it with the current set of substitutions
	 (filtered-ct-patterns (filter-patterns new-order ct-patterns used-letters)))
    (if (is-text-solved new-pt) new-pt
	(try-pattern-words (cdr new-order)
			   new-pt
			   (cdr filtered-ct-patterns)))))

;;; Try to solve each word in order-of-solving until the plaintext is fully-solved
(defun try-pattern-words (order-of-solving plaintext ct-patterns)
  (if (or (is-text-solved plaintext) (null order-of-solving)) nil
      (labels ((try-pattern-words-iter (pattern-words)
		 (if (null pattern-words) nil
		     (let ((result (try-pattern-word (car pattern-words)
						     order-of-solving
						     plaintext
						     ct-patterns)))
		       (if result result
			   (try-pattern-words-iter (cdr pattern-words)))))))
	(try-pattern-words-iter (car ct-patterns)))))

;;; Try to solve a cryptogram with word breaks by trying each possible dictionary word
(defun crack-substitution-with-word-breaks (ciphertext pattern-dict)
  (let* ((ct-split (split-text ciphertext))
	 (order-of-solving (make-common-chain ct-split))
	 (order-patterns (mapcar (lambda (w) (get-words-with-same-pattern w pattern-dict))
				 order-of-solving)))
    (try-pattern-words order-of-solving ct-split order-patterns)))
