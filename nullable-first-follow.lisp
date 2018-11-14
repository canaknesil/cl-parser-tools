(load "grammar.lisp")



;;; TODO: Make print outs human readible.
(defun print-sets (nullable first follow)
  (format t "nullable = ~a~%" nullable)
  (format t "FIRST = ~a~%" first)
  (format t "FOLLOW = ~a~%~%" follow))

(defun create-alist (keys init)
  (let* ((n (length keys))
	 (vals (make-list n :initial-element init)))
    (pairlis keys vals)))

(defmacro cpytree (var set)
  `(setf ,var (copy-tree ,set)))

;;; Assume that each data is a set.
(defun union-alist-data (alist key data)
  (let ((pair (assoc key alist)))
    (setf (cdr pair) (union (list data) (cdr pair)))))

(defun set-alist-data (alist key data)
  (let ((pair (assoc key alist)))
    (setf (cdr pair) data)))

(defun all-nullable-p (list nullable)
  (let ((all-null-p t))
    (loop for sym in list doing
	 (if (not (cdr (assoc sym nullable)))
	     (setf all-null-p nil)))
    all-null-p))

(defun segments-from-case-1 (syms nullable)
  ;; TODO
  nil)

(defun segments-from-case-2 (syms nullable)
  ;; TODO
  nil)

(defun segments-from-case-3 (syms nullable)
  ;; TODO
  nil)
	
;;;; INTERFACE

(defun nullable-first-follow (grammar)
  "Returns a property list with properties :nullable, :first, and :follow."
  (let ((terms (get-terminals grammar))
	(non-terms (get-non-terminals grammar))
	(all-syms (get-all-symbols grammar))
	(productions (get-production-list grammar)))

    ;; THE ALGORITHM AT LAST
    
    (let ((nullable (create-alist all-syms nil))
	  (first (create-alist all-syms nil))
	  (follow (create-alist all-syms nil))
	  (nullable-old (create-alist all-syms nil))
	  (first-old (create-alist all-syms nil))
	  (follow-old (create-alist all-syms nil)))

      (mapcar #'(lambda (ter) (union-alist-data first ter ter))
	      terms)
      
      (loop do
	   (cpytree nullable-old nullable)
	   (cpytree first-old first)
	   (cpytree follow-old follow)
	   (print-sets nullable first follow)

	   (mapcar
	    #'(lambda (p)
		(let ((lhs (car p))
		      (rhs (cdr p)))
		  
		    (if (or (eql rhs nil) (all-nullable-p rhs nullable))
			(set-alist-data nullable (car p) t))

		    ;; Iterate the rhs.
		    ;; 1: Try to find a non-nullable from left.
		    ;;    If find, perform case 1 and continue.
		    ;; 2: Try to find a second non-nullable.
		    ;;    If find, perform case 3.
		    ;;    If not, perform case 2.
		    ;; Case 1
		    (let ((segs (segments-from-case-1 rhs nullable)))
		      ;;TODO
		      nil)

		    (let ((segs (segments-from-case-2 rhs nullable)))
		      ;;TODO
		      nil)

		    (let ((segs (segments-from-case-3 rhs nullable)))
		      ;;TODO
		      nil)
		    
		    ))
	    
	    productions)

	 until (equal (list nullable first follow)
		      (list nullable-old first-old follow-old)))
      
      (list :nullable nullable
	    :first first
	    :follow follow))))
