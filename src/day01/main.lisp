; sbcl --script .\src\day01\main.lisp
(defun sum (xs) (reduce #'+ xs))
(defun string-split-one-space (s) 
    (loop for i = 0 then (1+ j)
          as j = (position #\Space s :start i)
          collect (subseq s i j)
          while j))
(defparameter whitespace '(#\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page #\Return #\Space #\Rubout #\Next-Line #\No-break_space))
(defparameter digit-strings (string-split-one-space "zero one two three four five six seven eight nine"))
(defparameter digits "0123456789")
(defun read-lines (path)
    (with-open-file (file path)
        (loop for line = (read-line file nil nil)
                until (eq line nil)
                collect (string-trim whitespace line))))

(defun p1-line (line)
    (+ (* (digit-char-p (find-if 'digit-char-p line)) 10) (digit-char-p (find-if 'digit-char-p line :from-end t))))

(defun p1 (lines) (sum (map 'list 'p1-line lines)))

(defun string-starts-with (prefix string) (eq 0 (search prefix string)))

; checks to see if a string begins with a digit word like "two"
; returns the word or nil
(defun digit-word-prefix (s)
    (loop for prefix in digit-strings
          do (if (string-starts-with prefix s)
                 (return prefix)
                 nil)))

; from a string like "xxtwo1fiveteensixxx" to 2 1 5 6
(defun p2-digits (s)
    (if (eq 0 (length s))
        nil
        (let ((c (digit-char-p (char s 0))))
            (if (not(eq c nil))
                (cons c (p2-digits (subseq s 1)))
                (let ((prefix (digit-word-prefix s)))
                    (if prefix
                        (cons (position prefix digit-strings) (p2-digits (subseq s (- (length prefix) 1))))
                        (p2-digits (subseq s 1))))))))

(defun p2-line (line)
    (let* ((digits (p2-digits line))
          (a (elt digits 0))
          (b (elt digits (- (length digits) 1))))
        (+ (* a 10) b)))

(defun p2 (lines) (sum (map 'list 'p2-line lines)))

(let ((lines (read-lines #p"puzzle_input\\day01")))
    (format t "part 1: ~a~%" (p1 lines))
    (format t "part 2: ~a~%" (p2 lines)))
