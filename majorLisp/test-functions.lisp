;;;=============================================================================
;;;Brent Turner
;;;Major Lisp Project
;;; TEST-Functions.lisp
;;;CSC345 - Dr. Wyatt
;;; Sept 27, 2017 - RWW

;;===================================================================
;; The below is for personal testing only.  Once the program runs and
;; is tested to programmer's satisfaction, it would be removed.  It is
;; included here as an illustration only.
;; 
;;;============================================================================
;;; functions to run the examples

(defun run-all-diffs (n)
  "Run differentiate on all functions from F1 to F<n>"
  (terpri)
  (run-all-diffs2 1 n))


(defun run-all-diffs2 (m n)
  "Helper function for RUN-DIFFS -- evaluate NICE-DIIF on F<m>--F<n>"
  (cond ((> m n) nil)
	(t (make-and-eval-differentiate-expression m)
	   (run-all-diffs2 (1+ m) n))))

(defun build-symbol (n)
  "Makes (and interns) the lisp symbol F<n>, for example: F1 or F2 or F3, etc. "
  (intern (concatenate 'string "F" (prin1-to-string n))))

(defun make-and-eval-differentiate-expression (n)
  "Build and evaluate the expression for differentiating 
F<n> w.r.t. variable x (or y if n=4)"
  (format t "DIFFERENTIATING FUNCTION F~a" n)
  (if (= n 4)(format t "               NB: differentiate here w.r.t. y, not x"))
  (terpri)
  (eval `(nice-diff ,(build-symbol n) (if (= ,n 4) 'y 'x))))

(defun nice-diff (F V)
  (format t "FUNCTION: ~a~%VARIABLE: ~a~%  
	RESULT: ~A~%~%" F V (differentiate F V)))

;;;===================================================================
;;;  THE TEST EXPRESSIONS

(defconstant f1  'a)
(defconstant f2  '(- - x))
(defconstant f3  '(- - - x))    
(defconstant f4  '(x / 5))       ;; NB: F4 is to be differentiated wrt y, not x
(defconstant f5  '((x + x) + x))
(defconstant f6  '(x - (- x)))
(defconstant f7  '((- x) + (- x)))
(defconstant f8  '(x / (2 * x)))
(defconstant f9  '((x * x) * (x * x)))
(defconstant f10 '(a * (x ** 3)))


;;;=============================================================================================
