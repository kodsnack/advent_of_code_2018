(defun get-lines (ist)
    (multiple-value-bind (line) (read-line ist nil)
        (if line
            (cons line (get-lines ist))
            '())))

(defun findMultiple (s i n)
  (cond ((= i (length s)) nil)
	((= (count (char s i) s) n) t)
	(t (findMultiple s (+ i 1) n))))

(defun sumMultiples (l i n)
  (cond ((endp l) 0)
	(t (+ (if (findMultiple (first l) i n) 1 0)
	      (sumMultiples (rest l) i n)))))

(defun removeCharFromStr (s idx)
  (concatenate 'string (subseq s 0 idx) (subseq s (+ idx 1))))

(defun removeCharFromList (lst idx)
  (cond ((endp lst) '())
	(t (cons (removeCharFromStr (first lst) idx)
		 (removeCharFromList (rest lst) idx)))))

(defun findDouble (lst)
  (cond ((endp lst) nil)
	((find (first lst) (rest lst) :test 'string=) (first lst))
	(t (findDouble (rest lst)))))

(defun findSimilar (lst idx)
  (cond ((= idx (length (first lst))) nil)
	(t (let ((found (findDouble (removeCharFromList lst idx))))
	     (if found found (findSimilar lst (+ idx 1)))))))

(setq sl (with-open-file (ifile "day02.txt" :direction :input)
			 (get-lines ifile)))

(write-line (format nil "Solution for day 2, part 1: ~D"
		    (* (sumMultiples sl 0 2)
		       (sumMultiples sl 0 3))))

(write-line (format nil "Solution for day 2, part 1: ~D"
		    (findSimilar sl 0)))
