; sbcl --script .\src\day02\main.lisp
(defun sum (xs) (reduce #'+ xs))
(defparameter whitespace '(#\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page #\Return #\Space #\Rubout #\Next-Line #\No-break_space))
(defun read-lines (path)
    (with-open-file (file path)
        (loop for line = (read-line file nil nil)
                until (eq line nil)
                collect (string-trim whitespace line))))
(defun string-starts-with (prefix string) (eq 0 (search prefix string)))
(defun split (delim s)
    (loop for i = 0 then (1+ j)
          as j = (position delim s :start i)
          if (or (not j) (/= j 0)) collect (subseq s i j)
          while j))

(defun needed-cubes (s) 
    (let* ((r 0)
           (g 0)
           (b 0))
        (loop
            for round in (split #\; s)
            while round
            do (loop
                for pair in (split #\, round)
                as parts = (split #\Space pair)
                do (let* ((value (parse-integer (nth 0 parts)))
                          (color (nth 1 parts)))
                    (if (string= "red" color) (setf r (max r value)) nil)
                    (if (string= "green" color) (setf g (max g value)) nil)
                    (if (string= "blue" color) (setf b (max b value)) nil))))
        (list r g b)))

(defun p1-score-line (line)
    (let* ((line (if (string-starts-with "Game " line)
                    (subseq line 5)
                    (line)))
           (i (position #\: line))
           (id (subseq line 0 i))
           (rest (subseq line (1+ i)))
           (rgb (needed-cubes rest)))
        (if (> (nth 0 rgb) 12)
            0
            (if (> (nth 1 rgb) 13)
                0
                (if (> (nth 2 rgb) 14)
                    0
                    (parse-integer id))))))

(defun p2-score-line (line)
    (let* ((i (position #\: line))
           (rgb (needed-cubes (subseq line (1+ i)))))
        (* (nth 0 rgb) (nth 1 rgb) (nth 2 rgb))))

(defun p1 (lines) (sum (map 'list 'p1-score-line lines)))

(defun p2 (lines) (sum (map 'list 'p2-score-line lines)))

(let ((lines (read-lines #p"puzzle_input\\day02")))
    (format t "~a~%" (p1 lines)) ; 2156
    (format t "~a~%" (p2 lines))) ; 66909
