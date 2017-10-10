;;constants
(defconstant constant-symbols '(A B C D E F G H N M))
(defconstant negation-symbol '-)
(defconstant variable-symbols '(U V W X Y Z))
(defconstant sum-symbol '+)
(defconstant product-symbol '*)
(defconstant division-symbol '/)
(defconstant subtraction-symbol '-)
(defconstant power-symbol '**)

;;actual function
(defun differentiate (F V)
  (cond ((constant-p F) (make-constant 0)) ;;if F is a constant eval 0

	((sum-p F)
	 (make-sum (differentiate (sum-operand-1 F) V) (differentiate (sum-operand-2 F) V)))

	((negation-p F)
	 (make-negation (differentiate (rest F) V)))

	((subtraction-p F)
	 (make-subtraction (differentiate (subtraction-operand-1 F ) V) (differentiate (sum-operand-2 F) V)))

	((product-p F)
	 (make-product (differentiate (product-operand-1 F) V) (differentiate (sum-operand-2 F) V)))

	((division-p F)
	 (make-division (differentiate (division-operand-1 F) V) (differentiate (division-operand-2 F) V)))

	((power-p F)
	 (make-power (differentiate (power-operand-1 F) V) (power-operand-2 F)))

	((variable-p F) (if (equal (make-variable F) (make-variable V))
			    (make-constant 1)
			    (make-constant 0)))))

(defun make-constant (C) C)
(defun make-variable (V)
  (cond ((listp V) (list (first V)))
	(t(list V))))

;;predicates
(defun constant-p (F)
  (or (numberp F) (member F constant-symbols)))

(defun variable-p (V)
  (cond ((member V variable-symbols) t)
	((and (listp V) (variable-p (first V)))
	 (member (first V) variable-symbols))))

(defun negation-p (F)
  (and (listp F)
       (equal (first F) negation-symbol)))

(defun sum-p (F)
  (and (listp F)
       (equal (sum-operator F) sum-symbol) (>= (length F) 3)))

(defun subtraction-p (F)
  (and (listp F)
       (equal (subtraction-operator F) subtraction-symbol) (>= (length F) 3)))

(defun product-p (F)
  (and (listp F)
       (equal (product-operator F) product-symbol) (>= (length F) 3)))

(defun division-p (F)
  (and (listp F)
       (equal (division-operator F) division-symbol) (>= (length F) 3)))

(defun power-p (F)
  (and (listp F)
       (equal (power-operator F) power-symbol) (>= (length F) 3)))

;;operators
(defun negation-operator (F) (first F))
(defun sum-operator (F) (second F))
(defun subtraction-operator (F) (second F))
(defun product-operator (F) (second F))
(defun division-operator (F) (second F))
(defun power-operator (F) (second F))

;;operands
(defun negation-operand (F) (second F))
(defun sum-operand-1 (F) (first F))
(defun sum-operand-2 (F) (third F))
(defun subtraction-operand-1 (F) (first F))
(defun subtraction-operand-2 (F) (third F))
(defun product-operand-1 (F) (first F))
(defun product-operand-2 (F) (third F))
(defun division-operand-1 (F) (first F))
(defun division-operand-2 (F) (third F))
(defun power-operand-1 (F) (first F))
(defun power-operand-2 (F) (third F))

(defun make-sum (F G)
  (cond ((eq 0 F) G)
	((eq 0 G) F)
	((and (numberp F) (numberp G)) (+ F G))
	(t(list F sum-symbol G))))
	
(defun make-subtraction (F G)
  (cond ((eq 0 F) (make-negation G))
	((eq 0 G) F)
	((and (numberp F) (numberp G)) (- F G))
	(t(list F subtraction-symbol G))))

(defun make-product (F G)
  (cond ((eq 0 F) 0)
	((eq 0 G) 0)
	((and (numberp F) (numberp G)) (* F G))
	(t(list F product-symbol G))))

(defun make-division (F G)
  (cond ((eq G 0) nil)
	((eq F 0) 0)
	((and (numberp F) (numberp G)) (/ F G))
	(t(list F division-symbol G))))

(defun make-negation (F)
  (cond ((numberp F) (- 0 F))
	(t(list negation-symbol F))))

(defun make-power (F G)
  (cond ((eq F 0) 0)
	((eq G 0) 1)
	((and (numberp F) (numberp G))
	 (* F (make-power F (- G 1)))) ;;;subtract 1 from G for reccursion
	(t(list F power-symbol G))))

