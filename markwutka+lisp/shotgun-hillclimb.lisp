(load "analysis.lisp")
(load "scoring.lisp")

;;; Create a random key, start with a key that doesn't do anything (maps x to x)
;;; and for each position in the key, pick a place to swap it with
(defun create-random-key ()
  (let ((key (make-array 26 :initial-contents
			 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))))
    (loop for i from 0 to 25 do
	 (let* ((dest (random 26))
		(keyval (aref key i))
		(keyother (aref key dest)))
	   (setf (aref key i) keyother)
	   (setf (aref key dest) keyval)))
    key))

;;; Compute all the possible pairs in the key that can be swapped
(defun compute-possible-swaps ()
  (mapcan (lambda (i)
	    (mapcar (lambda (j) (cons i j)) (remove-if (lambda (j) (>= i j)) alphabet)))
	  alphabet))

;;; Swap two items in a key
(defun swap (key a b)
  (let ((temp (aref key a)))
    (setf (aref key a) (aref key b))
    (setf (aref key b) temp)))

(defun copy-array (from to)
  (loop for i from 0 to (1- (length from)) do
       (setf (aref to i) (aref from i))))

;;; This is an ugly mess, but basically, we try swapping every possible pair in the
;;; key to see if that improves the overall score. If the best key is the same as
;;; the original key, then there were no swaps better than the original key, so
;;; we are done. Otherwise, take the new best key and again look for the best swap.
(defun hillclimb-start (start-key ciphertext dawg best-score)
  ;;; Create a local copy of the starting key
  (let ((key (map 'vector #'identity start-key))
	;;; Create a destination to decrypt into so we aren't allocating this every
	;;; time we try a decrypt
	(plaintext (make-array (length ciphertext) :initial-element 0)))
    (labels ((hillclimb-iter (swap best-key best-score)
	       ;;; if there are no more swaps and the best-key 
	       (if (null swap) (if (equalp best-key start-key) (values best-key best-score)
				   ;;; Otherwise, we made an improvement, try going through
				   ;;; the possible swaps again to find another improvement
				   (progn
				     (hillclimb-start (map 'vector #'identity best-key)
						      ciphertext dawg best-score)))
		   ;;; There's another swap to try...
		   (progn
		     ;;; Do the swap
		     (swap key (caar swap) (cdar swap))
		     ;;; Try decrypting with this key
		     (do-substitution-into ciphertext key plaintext)
		     ;;; Evaluate the resulting ciphertext
		     (let ((score (score-segment plaintext 0 dawg nil)))
		       ;;; Is this an improvement?
		       (if (> score best-score)
			   (progn
			     ;;; Save off the best key
			     (copy-array key best-key)
			     ;;; Undo the swap so we can try the next swap
			     (swap key (caar swap) (cdar swap))
			     ;;; Try the next swap
			     (hillclimb-iter (cdr swap) best-key score))
			   (progn
			     ;;; No, this one wasn't better, undo the swap
			     (swap key (caar swap) (cdar swap))
			     ;;; ... and try the next one
			     (hillclimb-iter (cdr swap) best-key best-score))))))))
      (hillclimb-iter (compute-possible-swaps) (map 'vector #'identity start-key) best-score))))

(defun hillclimb (start-key ciphertext dawg)
  (hillclimb-start start-key ciphertext dawg 0))

;;; We can break out of the shotgun-hillclimb if the words found in the decrypted plaintext
;;; amount to a certain percentage of the overall length.
(defun should-quit (key ciphertext dawg coverage-pct)
  (let* ((plaintext (do-substitution ciphertext key))
	 (coverage (score-segment plaintext 0 dawg t)))
    (>= (float (/ coverage (length ciphertext))) coverage-pct)))

;;; This function tries to guess an initial key by counting the characters in the ciphertext
;;; and then assigning the most common one to E, the next to T and so forth.
;;; This doesn't seem to work very well, though.
(defun guess-decrypt-key (ciphertext)
  (let* ((counts (count-letters ciphertext))
	 (count-pairs (list->vector (loop for i from 0 to 25 collect (cons (aref counts i) i))))
	 (decrypt-key (make-array 26 :initial-element 0)))
    (sort count-pairs (lambda (a b) (> (car a) (car b))))
    (loop for i from 0 to 25 do
	 (setf (aref decrypt-key (cdr (aref count-pairs i))) (cdr (aref English-Freq-Order i))))
    decrypt-key))

;;; This routine doesn't stop, it has no way to know it is done, it just prints out its progress and
;;; it is up to you to recognize that it is done. It picks a random key and then uses the hillclimb
;;; function to see if it can make improvements. If it was an improvement, it prints out the improved
;;; score, key, and decrypted plaintext, and then whether or not there was an improvement, it picks
;;; a new random key and tries again.
(defun shotgun-hillclimb (ciphertext dawg coverage-pct)
  (labels ((shotgun-hillclimb-iter (best-score)
	     (let ((key (create-random-key)))
	       (multiple-value-bind (best-key next-best-score)
		   (hillclimb key ciphertext dawg)
		 (if (> next-best-score best-score)
		     (format t "Score ~A with key ~A: ~A~%" next-best-score (string-from-26 (invert-key best-key))
			     (string-from-26 (do-substitution ciphertext best-key))))
		 (if (should-quit best-key ciphertext dawg coverage-pct)
		     best-key
		 ;;; Rather than have two calls to this, one for each branch of the previous if,
		 ;;; we just do a max of score and next-best-score
		     (shotgun-hillclimb-iter (max best-score next-best-score)))
		 ))))
    (shotgun-hillclimb-iter 0)))
