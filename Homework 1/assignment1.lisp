(defun my-replace (e1 e2 L)
  "~~~~~~~"
  (cond ((endp L) nil)
	((equal (first L) e1)(cons e2 (my-replace e1 e2 (rest L))))
	((if (listp (first L)) (my-replace e1 e2 (first L))))
	(t(cons (first L) (my-replace e1 e2 (rest L)))))))
  
