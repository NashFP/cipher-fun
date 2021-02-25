;;; This code uses a DAWG (directed acyclic word graph) to look for valid words.
;;; The format of the DAWG file is a common one used by different crossword games.
;;; The file is a series of 32-bit numbers that are encoded as:
;;;    child offset (22 bits) is-end-of-list (1 bit) is-end-of-word (1 bit) letter (8 bits)
;;; The top of the tree is the last 26 numbers in the file. The way to see if something is a word
;;; is that you start at the top of the tree and compare the letter in the word you are
;;; testing with the letter stored at your current position in the tree.
;;; If the letters match, you jump to the child offset in the tree, which gives you the letters
;;; that can follow after the letter you just matched.
;;; If the letters don't match, check the is-end-of-list flag. If it is true, then you have hit a
;;; dead end and the word doesn't match.
;;; If the is-end-of-word bit is set, then the letters you have matched so far are a valid word.
;;; For example, suppose you are trying to match the word "foods", and you have matched f-o-o-d
;;; so far. The is-end-of-word bit will be set on the entry that has the d, but you still need to
;;; jump to the child offset to see if s can come after it (spoiler alert: it can)
;;;
;;; The letters in the file on disk are lowercase, I convert them here to be in the range 0-25 to
;;; match all the other functions here.

(defstruct dawg-node
  child-index
  is-end-of-word
  is-end-of-list
  letter
  )

;;; Read a 4-byte node from the file
(defun read-node (stream)
  (let ((b0 (read-byte stream nil nil)))
    (if (null b0) nil
	(let* ((b1 (read-byte stream nil nil))
	       (b2 (read-byte stream nil nil))
	       (b3 (read-byte stream nil nil))
	       ;;; The lower two bits of b2 are the is-eol and is-eow bits, so shift
	       ;;; that byte right 2 bits to get the lower 6 bits of the child offset
	       ;;; b0 and b1 only shift 14 and 6 bits instead of 16 and 8 since the
	       ;;; offset is only a 22-bit number
	       (child-idx (+ (ash b0 14) (ash b1 6) (ash b2 -2)))
	       (is-eol (= (logand b2 2) 2))
	       (is-eow (= (logand b2 1) 1))
	       ;;; The letter in the file is lowercase, but we are using 0-25 as the range
	       ;;; for letters to make lookups easier
	       (l (- b3 97)))
	  ;;; Turn these values into a dawg-node structure
	  (make-dawg-node :child-index child-idx :is-end-of-word is-eow
			  :is-end-of-list is-eol :letter l)))))

;;; Load the dawg from the file (enable1.daw in the repository, but it is possible to
;;; have other dictionaries with different sets of words)
(defun load-dawg (filename) 
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (map 'vector #'identity
	 (loop for node = (read-node stream)
	    while node
	      collect node))))

;;; Return the offset of the top of the dawg, which is 26th from the end
(defun dawg-top (dawg)
  (- (length dawg) 26))

;;; Rather than just detecting whether something is a word, we want to be able to find the
;;; longest word at a given position.
(defun match-word-iter (w w-offset dawg dawg-offset longest longest-offset)
  ;;; Get the current node in the dawg
  (let ((node (aref dawg dawg-offset)))
    ;;; Does the letter at this node equal the next letter in the word we are matching?
    (if (= (dawg-node-letter node) (aref w w-offset))
	;;; If so, have we hit the end of the word (or buffer)?	
	(if (= (1+ w-offset) (length w))
	    ;;; If we have hit the end of the word/buffer, check to see if everything we
	    ;;; have matched so far is a valid word
	    (if (dawg-node-is-end-of-word node) (+ longest (- w-offset longest-offset) 1)
		;;; If not, return the longest match we found
		longest)
	    ;;; If not the end of the word, check to see if what has matched so far is
	    ;;; a word, and if so, update longest
	    (if (dawg-node-is-end-of-word node)
		(match-word-iter w (1+ w-offset) dawg
				 (dawg-node-child-index node)
				 (+ longest (- w-offset longest-offset) 1)
				 (1+ w-offset))
		;;; Otherwise, don't update longest
		(match-word-iter w (1+ w-offset) dawg
				 (dawg-node-child-index node) longest longest-offset)))
	(if (dawg-node-is-end-of-list node)
	    longest
	    (match-word-iter w w-offset dawg (1+ dawg-offset) longest longest-offset)))))

(defun match-word (w dawg)
  (match-word-iter w 0 dawg (dawg-top dawg) 0 0))

(defun match-word-from (w pos dawg)
  (match-word-iter w pos dawg (dawg-top dawg) 0 pos))

(defun is-word (w dawg)
  (= (match-word w dawg) (length w)))


