(load "dawg.lisp")

;;; We try scoring a buffer by looking for the longest word, and then adding the
;;; square of that length to the score for everything after it. We also try skipping
;;; the next letter and try to score after that. We cache the results for each particular
;;; position so we don't repeat calculations.
;;;
;;; Because we also want to know the raw number of how many letters are part of a word,
;;; we have a coverage option that just uses longest instead of the square of longest,
;;; which essentially just sums the lengths of all the words found.
(defun score-segment-iter (buf pos cache dawg coverage)
  (if (>= pos (length buf))
      ;;; There is no score for the end of the buffer
      0
      ;;; look up this position in the cache
      (let ((cached (aref cache pos)))
	;;; If there is a cached value, return it
	(if (>= cached 0) cached
	    ;;; Otherwise, find the longest word
	    (let ((longest (match-word-from buf pos dawg)))
	      (if (> longest 0)
		  ;;; Compute the score with this word and what follows it
		  (let* ((score (+ (if coverage longest (* longest longest))
				   (score-segment-iter buf (+ pos longest) cache dawg coverage)))
			 ;;; Also compute the score for skipping the letter at this
			 ;;; position
			 (skip-score (score-segment-iter buf (1+ pos) cache dawg coverage))
			 (best-score (max score skip-score)))
		    ;;; Cache the result
		    (setf (aref cache pos) best-score)
		    best-score)
		  ;;; If there was no word at this position, just get a score for skipping this letter
		  (let ((score (score-segment-iter buf (1+ pos) cache dawg coverage)))
		    ;;; and cache it
		    (setf (aref cache pos) score)
		    score)))))))

(defun score-segment (buf pos dawg coverage)
  (let ((cache (make-array (length buf) :initial-element -1)))
    (score-segment-iter buf pos cache dawg coverage)))
