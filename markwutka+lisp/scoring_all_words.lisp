(load "dawg.lisp")

;;; We try scoring a buffer by looking for the longest word, and then adding the
;;; square of that length to the score for everything after it. We also try skipping
;;; the next letter and try to score after that. We cache the results for each particular
;;; position so we don't repeat calculations.
;;;
;;; Because we also want to know the raw number of how many letters are part of a word,
;;; we have a coverage option that just uses longest instead of the square of longest,
;;; which essentially just sums the lengths of all the words found.
(defun score-segment-all-words-iter (buf pos cache dawg coverage)
  (labels ((score-word (word-len)
	     (+ (if coverage word-len (* word-len word-len))
		(score-segment-all-words-iter buf (+ pos word-len) cache dawg coverage))))    
    (if (>= pos (length buf))
      ;;; There is no score for the end of the buffer
	0
      ;;; look up this position in the cache
	(let ((cached (aref cache pos)))
	;;; If there is a cached value, return it
	  (if (>= cached 0) cached
	    ;;; Otherwise, find the words at this position
	      (let ((words (match-words-from buf pos dawg)))
		(let* ((skip-score (score-segment-all-words-iter buf (1+ pos) cache dawg coverage))
		       (best-score (apply #'max skip-score (mapcar #'score-word words))))
		  (setf (aref cache pos) best-score)
		  best-score)))))))

(defun score-segment-all-words (buf pos dawg coverage)
  (let ((cache (make-array (length buf) :initial-element -1)))
    (score-segment-all-words-iter buf pos cache dawg coverage)))
