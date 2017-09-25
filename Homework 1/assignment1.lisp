(defun my-replace (e1 e2 L)
  "This function will deep replace the char e1, if it exists in the list, with e2, for all occurrences of e1"

  ;;base or end of the list return nil
  (cond ((endp L) nil)

	;;if the item is present (e1 is equal to current Element) then replace by cons e2 onto the rest of the list
	((equal (first L) e1)(cons e2 (my-replace e1 e2 (rest L))))

	;;(deep replace) if list then cons the function, used on the list, with the rest of the current list
	((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
	
	;;if the current element is not equal to e1 then keep it on the list and continue down recurssion (worst case: multi-recurssion)
	(t(cons (first L) (my-replace e1 e2 (rest L))))))

(defun fibonacci (n)
  "This will implement the stupid double recursion of Fibonacci numbers"

  ;;account for the base case of fibonacci; to which all numbers are formed (0 & 1)
  (cond ((eql n 1) 0)
	((eql n 2) 1)

	;;if recurssion is not done then continue calling the function and then add once the base case is reached O(n) = n^2
	((+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
  
(defun fibonacci-TR (n)
  "This will implement the smarter tail recursion of fibonacci  numbers"

  ;;First I needed a function that had the ability to have an accumulator; therefore, labels was needed
  (labels ((fibonacci-aux (oneBehind twoBehind current)

	     ;;account for the base case of 0 & 1, then, when it gets to the end (n = 3), add and evaluate the accumulator
	(cond ((eql current 1) 0)
	      ((eql current 2) 1)
	      ((eql current 3)(+ oneBehind twoBehind))

	      ;;finally if the recursion is not done then call the function again, but with the accumulator O(n)=n
	      ((fibonacci-aux (+ oneBehind twoBehind) oneBehind (- current 1))))))
    ;;call the auxiliary function with the base case of fibonacci to which all numbers are made (0 & 1)
    (fibonacci-aux 1 0 n)))



 ;;(cond ((= n 1) 0)
;;	((= n 2) 1)
;;	(t(fibonacci-aux 1 0 n))))

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
