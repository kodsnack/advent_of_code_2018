(defun get-lines (ist)
    (multiple-value-bind (line) (read-line ist nil)
        (if line
            (cons line (get-lines ist))
            '())))

(defun print-list (l)
    (cond
        ((endp l) (prin1 "End!"))
        (t (progn (prin1 (first l))
                  (print-list (rest l))))
        ))

(prin1 (with-open-file (ifile "day02.txt" :direction :input)
                (get-lines ifile)))
