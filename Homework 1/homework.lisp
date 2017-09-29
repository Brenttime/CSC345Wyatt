;;Homework1.lisp
;;By Brent Turner
;;09/24/17

(defun my-replace (e1 e2 L)
  "This function will deep replace the char e1, if it exists in the list, with e2, for all occurrences of e1"

  ;;base or end of the list return nil
  (cond ((endp L) nil)

	;;if the item is present (e1 is equal to current Element) then replace by cons e2 onto the rest of the list
	((equal (first L) e1)(cons e2 (my-replace e1 e2 (rest L))))

	;;(deep replace) if list then cons the function, used on the list, with the rest of the current list
	((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
	
	;;if the current element is not equal to e1 then keep it on the list and continue down recursion (worst case: multi-recursion)
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
	      ((eql current 2)(+ oneBehind twoBehind))

	      ;;finally if the recursion is not done then call the function again, but with the accumulator O(n)=n
	      ((fibonacci-aux (+ oneBehind twoBehind) oneBehind (- current 1))))))
    
    ;;call the auxiliary function with the base case of fibonacci to which all numbers are made (0 & 1)
    (fibonacci-aux 1 0 n)))





;;; FOR USE IN HOMEWORK 1

;;;==================================================================================
;;; a macro to do FOR loops  ;; equivalent to Java for-loop control: for (int var=start, var<=stop; var+=update) body
(defmacro for ((var start stop update) &body body)
  (let ((gstop (gensym))                    ;; generate new symbols, GUARANTEED to be new; prevents capture
	(gupdate (gensym)))
    `(do ( (,gupdate ,update)               ;; needed so that the update expression is evaluated just once
	   (,var ,start (+ ,gupdate ,var))  ;; needed so that the stop expression is evaluated just once
	   (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;; EXAMPLE
;; CL-USER> (for (i 1 6 1) (print i))     ;; equivalent to Java for-loop control: (for int i=1, i<=6; i++)
;;
;; 1 
;; 2 
;; 3 
;; 4 
;; 5 
;; 6 
;; NIL

;;;==================================================================================
(defun comparefibonaccis()
  (for (i 10 35 5)                               ;; for (int i=10; i<=35, i+=5) ...
       (format t "TAIL REC FIBONACCI ~a~%" i)
       (time(fibonacci-TR i))
       (format t "FIBONACCI ~a~%" i)
       (time(fibonacci i))
       (format t "=======================================================~%")
       )
  )

;;;==================================================================================
;;; END 
