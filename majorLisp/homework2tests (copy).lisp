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
	 (make-subtraction
	  (differentiate (subtraction-operand-1 F ) V) (differentiate (sum-operand-2 F) V)))

;;	((product-p F)
	;;	 (make-product (product-operand-1 F) (product-operand-2 F) V))
	
	((product-p F) (make-sum
			(make-product
			 (product-operand-2 F) (differentiate (product-operand-1 F) V))
			(make-product
			 (product-operand-1 F) (differentiate (product-operand-2 F) V))))

;;	((division-p F)
	;;     	 (make-division (division-numerator F) (division-denominator F) V))
	
	((division-p F)
	 (make-division
	  (make-subtraction
	   (make-product (division-denominator F) (differentiate (division-numerator F) V))
	   (make-product (division-numerator F) (differentiate (division-denominator F) V)))
	 (list (division-denominator F) power-symbol 2)))
	
	;;redo power (make-power V (division-denominator F) 2)

	;;REDO POWER
	((power-p F)
	 (make-power (power-operand F) (power-exponent F) V))

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
(defun division-numerator (F) (first F))
(defun division-denominator (F) (third F))
(defun power-operand (F) (first F))
(defun power-exponent (F) (third F))

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
 ;; (cond ((eq 1 G) F) 
  ;;      (t(list (make-sum
  ;;(make-product G (differentiate F V) V) (make-product F (differentiate G V) V))))))
   ;; (t
;;;  (list
;;;   (list G product-symbol (differentiate F V))
;;;   sum-symbol (list F product-symbol (differentiate G V)))))
  (cond ((eq 1 G) F)
	((eq 1 F) G)
	((eq 0 F) 0)
	((eq 0 G) 0)
	((and (numberp F) (numberp G)) (* F G))
	(t(list F product-symbol G))))
	
(defun make-division (F G)
  (cond ((eq G 0) nil) ;;account for divide by zero error
	((and (numberp G) (numberp F)) (/ F G))
	(t(list F division-symbol G))))
	
;;	((numberp V) (/ (make-subtraction (make-product V Du) (make-product U Dv)) (make-product V V)))     
;;	(t(list (make-subtraction (make-product V Du) (make-product U Dv)) division-symbol (list V power-symbol 2))))) 
;;	((and (numberp F) (numberp G) (/ (make-subtraction (make-product G (differentiate F V) V)
;;					  (make-product F (differentiate G V) V)) (make-product G G V)))
;;	((and (numberp F) (numberp G) (/ (make-subtraction (* G (differentiate F V))
;;					 (* F (differentiate G V))) (* G G))))))
;;	(t(list (list (list G product-symbol (differentiate F V)) subtraction-symbol
;;		      (list F product-symbol (differentiate G V)))
;; division-symbol (list G power-symbol 2)))))

(defun make-negation (F)
  (cond ((numberp F) (- 0 F))
	(t(list negation-symbol F))))

(defun make-power (F G)
  (cond ((eq F 0) 0)
	((eq G 0) 0)
;;	((numberp G) (list G product-symbol
;;			   (list F power-symbol (1- G)) product-symbol (differentiate F V)))
;;        (t(list G product-symbol
;;		(list F power-symbol (make-subtraction G 1)) product-symbol (differentiate F V)))))
