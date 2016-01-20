;;; Find the segment of a sequence integer numbers with largest sum
;;; Algorithm in "Proofs as programs" by J. L. Bates and R. L. Constable,
;;; ACM Transactions on Programming Languages and Systems, Vol 7, Issue 1, Jan 1985
;;; Not optimized.

(defun max-L (s)
  (let ((len (length s))
        (last-element (first (last s))))
    (cond ((eql 0 len) 0)
          ((eql 1 len) (first s))
          (t (max (+ (max-L (subseq s 0 (- len 1)))
                     last-element)
                  last-element)))))

(defun mas-seg (s)
  (let ((len (length s)))
    (cond ((eql 0 len) 0)
          ((eql 1 len) (first s))
          (t (max (mas-seg (subseq s 0 (- len 1)))
                  (max-L s))))))

(defun test ()
  (let ((data '(((1 2 3 -5 10) 11) ((1 2 3 -8 10) 10) ((1 2 3 -8) 6))))
    (format t "~&Tests pass? ~A~%" (reduce #'(lambda (x y)
		;(format t "~&~A; ~A; ~A" y (mas-seg (first y)) (second y))
		(and x (equal (mas-seg (first y))
			      (second y))))
	    data
	    :initial-value t))))

(test)
