(load "analysis.lisp")

(defun encrypt-vigenere (key-str plaintext-str)
  (let ((key (string-to-26 key-str))
	(plaintext (string-to-26 plaintext-str)))
    (poly-join (mapcar #'encrypt-with-key-char (poly-split plaintext (length key)) key))))

(defun decrypt-vigenere (key ciphertext)
  (poly-join (mapcar #'decrypt-with-key-char (poly-split ciphertext (length key)) key)))


(defun determine-key-length-iter (ciphertext curr-length max-length best-ic best-length)
  ;;; If the current length > max-length, we have tried all the lengths, return the best
  (if (> curr-length max-length) best-length
      ;;; Compute the IC when the text is split curr-length ways
      (let ((per-ic (ic-at-period ciphertext curr-length)))
	(if (> per-ic best-ic)
	    ;;; If this IC was better than the current best, use it as the new best
	    (determine-key-length-iter ciphertext (1+ curr-length) max-length
				       per-ic curr-length)
	    (determine-key-length-iter ciphertext (1+ curr-length) max-length
				       best-ic best-length)))))

(defun determine-key-length (ciphertext max-length)
  (determine-key-length-iter ciphertext 1 max-length 0 1))

(defun determine-key-iter (ciphertext key best-cc best-key)
  ;;; If we have tried all 26 keys, return the best one
  (if (>= key 26) best-key
      ;;; Try decrypting with the current key
      (let* ((decrypted (decrypt-with-key-char ciphertext key))
	     ;;; See how well it correlates to English letter frequencies
	     (cc (correlate (count-letters-pct decrypted) English-Frequencies)))	
	(if (> cc best-cc)
	    ;;; If the correlation is better than the previous best, make it the new best
	    (determine-key-iter ciphertext (1+ key) cc key)
	    (determine-key-iter ciphertext (1+ key) best-cc best-key)))))

(defun determine-key (ciphertext)
  (determine-key-iter ciphertext 0 0 0))

(defun crack-vigenere (ciphertext)
  (let* ((key-length (determine-key-length ciphertext 10))
	 (splits (poly-split ciphertext key-length))
	 (key (mapcar #'determine-key splits)))
    (decrypt-vigenere key ciphertext)))
