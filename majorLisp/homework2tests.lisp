;;;=========================================================================================
;;; Differentiate Function
(defun differentiate (F V)
  (cond ((constant-p F) (make-constant 0)) ;;if F is a constant eval 0

	((sum-p F) ;;if sum
	 (make-sum (differentiate (sum-operand-1 F) V) (differentiate (sum-operand-2 F) V))) 

	((negation-p F) ;;if negation
	 (make-negation (differentiate (rest F) V)))

	((subtraction-p F) ;;if difference/subtraction
	 (make-subtraction
	  (differentiate (subtraction-operand-1 F ) V) (differentiate (sum-operand-2 F) V)))

	((product-p F)   ;;if product
	 (make-sum (make-product
		    (product-operand-2 F) (differentiate (product-operand-1 F) V))
		   (make-product
		    (product-operand-1 F) (differentiate (product-operand-2 F) V))))

	((division-p F)  ;;if division
	 (make-division
	  (make-subtraction
	   (make-product (division-denominator F) (differentiate (division-numerator F) V))
	   (make-product (division-numerator F) (differentiate (division-denominator F) V)))
	  (make-power (division-denominator F) 2)))

	((power-p F) ;;if power
	 (make-product (make-product
			(power-exponent F)
			(make-power (power-operand F) (1- (power-exponent F))))
		       (differentiate (power-operand F) V)))
	
	((variable-p F) ;;if variable
	 (if (equal (make-variable F) (make-variable V))
			    (make-constant 1)
			    (make-constant 0)))))

;;;============================================================================
;;; SYMBOLS: Only Used Symbols
(defconstant constant-symbols '(A B C D E F G H N M))
(defconstant negation-symbol '-)
(defconstant variable-symbols '(U V W X Y Z))
(defconstant sum-symbol '+)
(defconstant product-symbol '*)
(defconstant division-symbol '/)
(defconstant subtraction-symbol '-)
(defconstant power-symbol '**)

;;;=========================================================================================
;;;SELECTORS -- OPERATORS
(defun negation-operator (F) (first F))
(defun sum-operator (F) (second F))
(defun subtraction-operator (F) (second F))
(defun product-operator (F) (second F))
(defun division-operator (F) (second F))
(defun power-operator (F) (second F))

;;; SELECTORS -- OPERANDS
(defun negation-operand (F) (second F))
(defun sum-operand-1 (F) (first F)) 
(defun sum-operand-2 (F) (third F))
(defun subtraction-operand-1 (F) (first F))
(defun subtraction-operand-2 (F) (third F))
(defun product-operand-1 (F) (first F))
(defun product-operand-2 (F) (third F))
(defun division-numerator (F) (first F))
(defun division-denominator (F) (third F))
(defun power-operand (F) (first F))
(defun power-exponent (F) (third F))


;;;=============================================================================
;;; PREDICATES

;;;if constant
(defun constant-p (F)
  (or (numberp F) (member F constant-symbols)))

;;;if variable
(defun variable-p (V)
  (cond ((member V variable-symbols) t)
	((and (listp V) (variable-p (first V)))
	 (member (first V) variable-symbols))))

;;;if negation
(defun negation-p (F)
  (and (listp F)
       (equal (first F) negation-symbol)))

;;;if sum
(defun sum-p (F)
  (and (listp F)
       (equal (sum-operator F) sum-symbol) (>= (length F) 3)))

;;;if subtraction
(defun subtraction-p (F)
  (and (listp F)
       (equal (subtraction-operator F) subtraction-symbol) (>= (length F) 3)))

;;;if product
(defun product-p (F)
  (and (listp F)
       (equal (product-operator F) product-symbol) (>= (length F) 3)))

;;;if division
(defun division-p (F)
  (and (listp F)
       (equal (division-operator F) division-symbol) (>= (length F) 3)))

;;;if power
(defun power-p (F)
  (and (listp F)
       (equal (power-operator F) power-symbol) (>= (length F) 3)))

;;;=========================================================================================
;;; CONSTRUCTORS

(defun make-constant (C) C)

(defun make-variable (V)
  (cond ((listp V) (list (first V)))
	(t(list V))))

(defun make-sum (F G)
  (cond ((eq 0 F) G) ;;X + 0 = X
	((eq 0 G) F) ;;0 + X = X
	((and (numberp F) (numberp G)) (+ F G))
	(t(list F sum-symbol G))))
	
(defun make-subtraction (F G)
  (cond ((eq 0 F) (make-negation G)) ;;0 - X = - X
	((eq 0 G) F) ;;X - 0 = X
	((eq F G) 0) ;;X - X = 0
	((and (numberp F) (numberp G)) (- F G))
	(t(list F subtraction-symbol G))))

(defun make-product (F G)
  (cond ((eq 1 G) F) ;; X * 1 = X
	((eq 1 F) G) ;; 1 * x = x
	((eq 0 F) 0) ;; 0 * X = 0
	((eq 0 G) 0) ;; x * 0 = 0
	((and (numberp F) (numberp G)) (* F G))
	(t(list F product-symbol G))))
	
(defun make-division (F G)
  (cond ((eq G 0) nil) ;;account for divide by zero error
	((and (numberp G) (numberp F)) (/ F G))
	(t(list F division-symbol G))))
	
(defun make-negation (F)
  (cond ((numberp F) (- 0 F)) ;;-1 = -1
	(t(list negation-symbol F))))

(defun make-power (F G)
  (cond ((eq F 0) 0) ;;0 ** 1 = 0
	((eq G 0) 1) ;;x ** 0 = 1
	((and (numberp G) (numberp F))
	 (* F (make-power F (- G 1)))) ;;multiply F recursively
	(t(list F power-symbol G))))
