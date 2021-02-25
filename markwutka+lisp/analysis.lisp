;;; Frequency count of english letters
(defvar English-Frequencies
  #(0.0805 0.0170 0.0240 0.0430 0.1244 0.0222 0.0208 0.0647
    0.0699 0.0016 0.0080 0.0410 0.0264 0.0690 0.0753 0.0175            
    0.0012 0.0563 0.0632 0.0902 0.0281 0.0094 0.0239 0.0013            
    0.0200 0.0009))

;;; Sorted list of english frequency count and letter in descendin order
(defvar English-Freq-Order
  (map 'vector #'identity
       (sort (loop for i from 0 to 25 collect (cons (aref English-Frequencies i) i))
				(lambda (a b) (> (car a) (car b))))))

(defvar Alphabet '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

(defun read-file-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
	 ;;; I append a space here because I expect to join everything together
       collect (concatenate 'string line " "))))

(defun read-file (filename)
  (apply #'concatenate 'string (read-file-lines filename)))

(defun list->vector (l)
  (map 'vector #'identity l))

;;; Convert a character to a number in the range 0-25
(defun to-26 (ch)
  (cond ((and (char>= ch #\A) (char<= ch #\Z))
	 (- (char-code ch) (char-code #\A)))
	((and (char>= ch #\a) (char<= ch #\z))
	 (- (char-code ch) (char-code #\a)))
	(t nil)))

;;; Convert a number in the range 0-25 to an uppercase letter
(defun from-26 (i)
  (cond ((and (>= i 0) (<= i 25))
	 (code-char (+ i (char-code #\A))))
	(t nil)))

(defun is-AZ (ch)
  (or (and (char>= ch #\A) (char<= ch #\Z))
      (and (char>= ch #\a) (char<= ch #\z))))

;;; Converts a string to a list of numbers in the range 0-25, ignoring
;;; anything that isn't a letter in the range A-Z (or a-z)
(defun string-to-26 (s)
  (mapcar #'to-26 (remove-if-not #'is-AZ (map 'list #'identity s))))

;;; Converts a list of numbers in the range 0-25 to a string of letters
(defun string-from-26 (l)
  (map 'string #'from-26 l))

;;; Returns a vector containing the counts of each letter
(defun count-letters (l)
  (let ((counts (make-array 26 :initial-element 0)))
    (if (arrayp l)
	(loop for i across l do
	     (setf (aref counts i) (1+ (aref counts i))))
	(loop for i in l do
	     (setf (aref counts i) (1+ (aref counts i)))))
    counts))

;;; Returns a vector containing the counts of each letter as a percentage
;;; of the total number of letters
(defun count-letters-pct (l)
  (let ((num-letters (length l)))    
    (map 'vector (lambda (c) (float (/ c num-letters))) (count-letters l))))

(defun sqr (x) (* x x))

;;; Computes a correlation coefficient between two vectors of counts
(defun correlate (c1 c2)
  (let ((num-pcts (length c1))
	(sx (apply #'+ (map 'list #'identity c1)))
	(sx2 (apply #'+ (map 'list #'sqr c1)))
	(sy (apply #'+ (map 'list #'identity c2)))
	(sy2 (apply #'+ (map 'list #'sqr c2)))
	(sxy (apply #'+ (map 'list #'* c1 c2))))
    (float (/ (- sxy (float (/ (* sx sy) num-pcts)))
	      (sqrt (* (- sx2 (float (/ (sqr sx) num-pcts)))
		       (- sy2 (float (/ (sqr sy) num-pcts)))))))))

;;; Computes the Index of Coincidence for a text
(defun compute-ic (l)
  (let* ((counts (count-letters l))
	 (num-letters (length l))
	 (fsums (map 'list (lambda (c) (* c (1- c))) counts)))
    (float (/ (apply #'+ fsums) (* num-letters (1- num-letters))))))

(defun poly-split-iter (l period pos lists)
  (if (null l)
      (map 'list #'reverse lists)
      (progn
	(setf (aref lists pos) (cons (car l) (aref lists pos)))
	(poly-split-iter (cdr l) period (mod (1+ pos) period) lists))))

;;; Splits a list as it would be split for a polyalphabetic cipher
(defun poly-split (l period)
  (let ((lists (make-array period :initial-element nil)))
    (poly-split-iter l period 0 lists)))

(defun poly-join-iter (lists accum)
  (if (null lists) (reverse accum)      
      (poly-join-iter (remove-if-not #'identity (mapcar #'cdr lists))
		      (append (remove-if-not #'identity (mapcar #'car lists)) accum))))

;;; Joins lists that were split using poly-split.
;;; poly-join (poly-split l n) should equal l for any n >= 1
(defun poly-join (lists)
  (poly-join-iter (reverse lists) '()))

;;; Compute the IC for a poly split by splitting and then averaging the IC in each split
(defun ic-at-period (l period)
  (let ((splits (poly-split l period)))
    (float (/ (apply #'+ (mapcar #'compute-ic splits)) period))))

(defun encrypt-char-with-key-char (ch key-ch)
  (mod (+ ch key-ch) 26))

(defun encrypt-with-key-char (l key-ch)
  (mapcar (lambda (ch) (encrypt-char-with-key-char ch key-ch)) l))

(defun decrypt-char-with-key-char (ch key-ch)
  (mod (- (+ ch 26) key-ch) 26))

(defun decrypt-with-key-char (l key-ch)
  (mapcar (lambda (ch) (decrypt-char-with-key-char ch key-ch)) l))

;;; Insert a letter into a key if it isn't already there
(defun insert-into-key (key-bytes key key-pos)
  (if (null key-bytes) key
      (if (= -1 (aref key key-pos))
	  (progn (setf (aref key key-pos) (car key-bytes))
		 (insert-into-key (cdr key-bytes) key 0))
	  (if (= (aref key key-pos) (car key-bytes))
	      (insert-into-key (cdr key-bytes) key 0)
	      (insert-into-key key-bytes key (1+ key-pos))))))

;;; Shift a key over by a certain number of positions
(defun shift-key (key at)
  (let ((new-key (make-array 26 :initial-element -1)))
    (loop for i from 0 to (1- (length key)) do
	 (setf (aref new-key (mod (+ i at) 26)) (aref key i)))
    new-key))

;;; Create a key from a key word (or phrase) by taking each unique letter
;;; from the key word and inserting it at the beginning of a key, then
;;; appending any letters that are still missing from the key. Then shift
;;; the key over by the number of positions specified by at.
;;; For example, (make-key "lisp" 0) creates the key
;;; LISPABCDEFGHJKMNOQRTUVWXYZ
;;; while (make-key "lisp" 12) creates the key
;;; MNOQRTUVWXYZLISPABCDEFGHJK
;;; Thus, the (make-key "lisp" 12) maps A->M, B->N, C->O, D->Q ... K->Z
(defun make-key (key-str at)
  (let ((key-bytes (string-to-26 key-str))
	(key (make-array 26 :initial-element -1)))
    (insert-into-key key-bytes key 0)
    (insert-into-key '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25) key 0)
    (if (= 0 at) key
	(shift-key key at))))

;;; Inverts a key, so that substituting with the inverted key undoes the substitution
;;; done with the original key so that
;;; (do-substitution (do-substitution plaintext (make-key something pos)) (make-decrypt-key something pos))
;;; should return the original plaintext
(defun invert-key (key)
  (let ((inverted-key (make-array 26 :initial-element -1))	)
    (loop for i from 0 to (1- (length key)) do
	 (setf (aref inverted-key (aref key i)) i))
    inverted-key))

;;; Creates an inverse key by creating a key with make-key and then creating the inverse
(defun make-decrypt-key (key-str at)
  (let ((key (make-key key-str at)))
    (invert-key key)))

;;; Performs a substitution of letters in l using the key
(defun do-substitution (l key)
  (map 'vector (lambda (ch) (aref key ch)) l))

(defun do-substitution-into (l key dest)
  (loop for i from 0 to (1- (length dest)) do
       (setf (aref dest i) (aref key (aref l i)))))
