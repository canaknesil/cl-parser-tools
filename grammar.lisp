


;;; The implementation seems messy but this function simply performs
;;; all the checks on the input and returns it.
(defun create-grammar-f (production-list)
  (if (eql production-list nil) (error "Grammar must not be empty!"))
  (mapcar
   #'(lambda (p)
       (unless (listp p)
	 (error "Productions must be lists!~%~%~a~%" p))
       (if (eql p nil)
	   (error "Productions must not be empty!"))
       (mapcar
	#'(lambda (s)
	    (if (listp s)
		(error "Terminals and non-terminals must not be
lists!~%~%~a~%" s)))
	p))
   production-list)
  production-list)

;;;; INTERFACE

(defmacro create-grammar (&rest productions)
  "Returns the grammer abstract data structure according to the
specification, which is one or more production. Example
production: (T F A) where T is the left hand side, F and A are the 
right hand side. The left hand side of the first production is the 
start symbol."
  `(create-grammar-f '(,@productions)))


(defun get-start-symbol (grammar)
  "Returns the start symbol of the grammar."
  (caar grammar))

(defun get-production-list (grammar)
  "Returns the list of productions where each production is a list 
composed of the left hand side of the production followed by the 
right hand side."
  grammar)

(defun get-productions-with (sym grammar)
  "Returns productions of the non-terminal sym."
  (remove-if-not #'(lambda (p) (eql (car p) sym))
		 grammar))

(defun get-non-terminals (grammar)
  "Return a list of non-terminals in the grammar."
  (remove-duplicates
   (mapcar #'(lambda (p) (car p))
	   grammar)))

(defun get-all-symbols (grammar)
  "Returns all the symbols in the grammar."
  (let ((syms nil))
    (mapcar #'(lambda (p) (setf syms (append syms p)))
	    grammar)
    (remove-duplicates syms)))

(defun get-terminals (grammar)
  "Returns all the terminals in the grammer."
  (set-difference (get-all-symbols grammar)
		  (get-non-terminals grammar)))
      
