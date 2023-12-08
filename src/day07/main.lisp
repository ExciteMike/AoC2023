; sbcl --script .\src\day07\main.lisp
(load "~/quicklisp/setup.lisp") 
(ql:quickload "trivia")
(use-package :trivia)

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
(defun enumerate (xs)
  (loop for x in xs for i from 0 collect (list i x)))
(defun println (value)
  (format t "~s~%" value))
(defun seconds (xs) (map 'list #'second xs))
(defun character-histogram (xs)
  (let ((counter (make-hash-table)))
    (loop
      for x across xs
      do
        (setf (gethash x counter)
          (1+ (gethash x counter 0))))
    counter))

(defun parse-line (line)
    (let ((parts (split #\Space line)))
        (list (first parts) (parse-integer (second parts)))))

; values for type of hand
(defvar FIVE-OF-A-KIND 7)
(defvar FOUR-OF-A-KIND 6)
(defvar FULL-HOUSE 5)
(defvar THREE-OF-A-KIND 4)
(defvar TWO-PAIR 3)
(defvar ONE-PAIR 2)
(defvar HIGH-CARD 1)

(defun card-value (c)
  (cond ((eql c #\A) 14)
        ((eql c #\K) 13)
        ((eql c #\Q) 12)
        ((eql c #\J) 11)
        ((eql c #\T) 10)
        (t           (digit-char-p c 10))))
          
(defun card-value-p2 (c)
  (cond ((eql c #\J) 1)
        (t (card-value c))))

(defun counter-to-sorted-pairs-desc (counter)
  (sort (loop for k being the hash-key of counter
                  collect (list k (gethash k counter)))
        #'>
        :key #'second))

(defun hand-type (hand)
  (let ((counter (character-histogram hand)))
    (match (counter-to-sorted-pairs-desc counter)
      ((list* (list _ 5) _)            FIVE-OF-A-KIND)
      ((list* (list _ 4) _)            FOUR-OF-A-KIND)
      ((list* (list _ 3) (list _ 2) _) FULL-HOUSE)
      ((list* (list _ 3) _)            THREE-OF-A-KIND)
      ((list* (list _ 2) (list _ 2) _) TWO-PAIR)
      ((list* (list _ 2) _)            ONE-PAIR)
      ((list* (list _ 1) _)            HIGH-CARD)
      ((list* _)                       FIVE-OF-A-KIND)
    )))

(defun hand-type-p2 (hand)
  (let* ((counter (character-histogram hand))
         (j-count (or (gethash #\J counter) 0)))
    (remhash #\J counter)
    (let-match (( (list most second-most)
                  (match (counter-to-sorted-pairs-desc counter)
                    ((list* (list _ a) (list _ b) _) (list a b))
                    ((list* (list _ a) _)            (list a 0))
                    ((list* _)                       (list 0 0)))))
      (match (list (+ most j-count) second-most)
        ((list 5 _) FIVE-OF-A-KIND)
        ((list 4 _) FOUR-OF-A-KIND)
        ((list 3 2) FULL-HOUSE)
        ((list 3 _) THREE-OF-A-KIND)
        ((list 2 2) TWO-PAIR)
        ((list 2 _) ONE-PAIR)
        ((list 1 _) HIGH-CARD)))))

(defun hand-value (hand)
  (cons (hand-type hand) (map 'list 'card-value hand)))

(defun hand-value-p2 (hand)
  (cons (hand-type-p2 hand) (map 'list 'card-value-p2 hand)))

(defun list-less-than (left right)
  (cond ((and (null left) (null right))
         t)
        ((> (first left) (first right))
         nil)
        ((< (first left) (first right))
         t)
        (t
         (list-less-than (rest left) (rest right))))) 

(defun score-sorted-bid (index+bid)
  (let ((index (first index+bid))
        (bid (second index+bid)))
        (* (1+ index) bid)))

(defun score-sorted-bids (bids)
  (sum (map 'list #'score-sorted-bid (enumerate bids))))

(defun p1-sort (pairs)
  (sort pairs #'list-less-than :key (lambda (hand+bid) (hand-value (first hand+bid)))))

(defun p2-sort (pairs)
  (sort pairs #'list-less-than :key (lambda (hand+bid) (hand-value-p2 (first hand+bid)))))

(let* ((hands (map 'list #'parse-line (read-lines #p"puzzle_input\\day07"))))
    (println (score-sorted-bids (seconds (p1-sort (copy-seq hands))))) ; 253866470
    (println (score-sorted-bids (seconds (p2-sort hands))))) ; 254494947
