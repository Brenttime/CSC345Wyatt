(defun my-replace (e1 e2 L)
  "This function will deep replace the char e1, if it exists in the list, with e2, for all occurrences of e1"
  (cond ((endp L) nil)
	((equal (first L) e1)(cons e2 (my-replace e1 e2 (rest L))))
	((if (listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L)))))
	(t(cons (first L) (my-replace e1 e2 (rest L))))))

(defun fibonacci (n)
  "This will implement the stupid double recursion of Fibonacci numbers"
  (cond ((eql n 1) 0)
	((eql n 2) 1)
	((+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
  
(defun fibonacci-TR (n)
  "This will implement the smarter tail recursion of fibonacci  numbers"
  (labels fibonacci-aux (oneBehind twoBehind current)
    (if (= current 3)
	(+ oneBehind twoBehind)
	(fibonacci-aux (+ oneBehind twoBehind) oneBehind (- current 1))))
  (if (< n 3) (if (= n 1) 0 1) (fibonacci-aux 1 0 n)))

;;These below functions start the fibonacci sequence at 1 rather than at 0 like above

;;(defun fibonacci (n)
  ;;"This will implement the stupid double recursion of Fibonacci numbers"
  ;;(cond ((eql n 1) 1)
;;	((eql n 2) 1)
;;	((+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
  
;;(defun fibonacci-TR (n)
  ;;"This will implement the smarter tail recursion of fibonacci  numbers"
  ;;(defun fib-aux (oneBehind twoBehind current)
    ;;(if (= current n)
	;;(+ oneBehind twoBehind)
	;;(fib-aux (+ oneBehind twoBehind) oneBehind (+ 1 current))))
  ;;(if (< n 3) 1 (fib-aux 1 1 3)))
