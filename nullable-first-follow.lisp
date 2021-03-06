(load (merge-pathnames "grammar.lisp" *load-truename*))


(defun print-set (name index list)
  (format t "~a(~a) = {" name index)
  (unless (eql list nil)
    (format t "~a" (car list))
    (mapcar #'(lambda (i)
		(format t ", ~a" i))
	    (cdr list)))
  (format t "}~%"))

(defun print-sets (nullable first follow)
  (mapcar #'(lambda (p)
	      (format t "nullable(~a) = ~a~%"
		      (car p) (if (eql (cdr p) nil) "f" "t")))
	  nullable)
  (mapcar #'(lambda (p) (print-set "FIRST" (car p) (cdr p))) first)
  (mapcar #'(lambda (p) (print-set "FOLLOW" (car p) (cdr p))) follow)
  (format t "~%"))

(defun create-alist (keys init)
  (let* ((n (length keys))
	 (vals (make-list n :initial-element init)))
    (pairlis keys vals)))

;;; Assume that each data is a set.
(defun add-alist-data (alist key data)
  (let ((pair (assoc key alist)))
    (setf (cdr pair) (union data (cdr pair)))))

(defun add-alist-data-atom (alist key atom)
  (add-alist-data alist key (list atom)))

(defun union-alist-data (alist-1 key-1 alist-2 key-2)
  (add-alist-data alist-1 key-1 (cdr (assoc key-2 alist-2))))

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
  "Returns list of symbols in syms whose left is all nullable."
  (if (eql syms nil) nil
      (let ((sym (car syms)))
	(if (cdr (assoc sym nullable))
	    (cons sym (segments-from-case-1 (cdr syms) nullable))
	    (cons sym nil)))))

(defun segments-from-case-2 (syms nullable)
  "Returns list of symbols in syms whose right is all nullable."
  (segments-from-case-1 (reverse syms) nullable))

(defun gen-pair-with-sym-and-list (sym list)
  (if (eql list nil) nil
      (cons (cons sym (car list))
	    (gen-pair-with-sym-and-list sym (cdr list)))))

(defun segments-from-case-3 (syms nullable)
  "Returns a list of pair of symbols with zero or more nullable symbols between them."
  (if (eql syms nil) nil
      (let ((sym (car syms))
	    (rest (cdr syms)))
	(append (gen-pair-with-sym-and-list
		 sym
		 (segments-from-case-1 rest nullable))
		(segments-from-case-3 rest nullable)))))

(defun set-equal (s1 s2)
  (not (set-difference s1 s2)))

(defun alist-of-set-equal (a1 a2)
  (let ((is-equal t))
    (loop for p in a1 doing
	 (let* ((k (car p))
		(v (cdr p))
		(p2 (assoc k a2)))
	   (unless p2
	     (setf is-equal nil)
	     (return))
	   (unless (if (listp v)
		       (set-equal v (cdr p2))
		       (eql v (cdr p2)))
	     (setf is-equal nil)
	     (return))))
    is-equal))

(defun make-gensym-plist (keys)
  (let ((plist nil))
    (loop for k in keys doing
	 (setf plist (append plist (list k (gensym)))))
    plist))

(defmacro until-no-change-at ((&rest objs) copy-fun compare-fun &body body)
  (let ((old-sym (make-gensym-plist objs))
	(copy-fun-sym (gensym))
	(compare-fun-sym (gensym)))
    `(let (,@(loop for obj in objs collecting
		  `(,(getf old-sym obj) 'dummy))
	   (,copy-fun-sym ,copy-fun)
	   (,compare-fun-sym ,compare-fun))
       (loop do
	    ,@(loop for obj in objs collecting
		   `(setf ,(getf old-sym obj) (funcall ,copy-fun-sym ,obj)))
	    ,@body
	  until (and ,@(loop for obj in objs collecting
			    `(funcall ,compare-fun-sym
				      ,obj
				      ,(getf old-sym obj))))))))
     
	
;;;; INTERFACE

(defun nullable-first-follow (grammar)
  "Returns a property list with properties :nullable, :first, and :follow."
  (let ((terms (get-terminals grammar))
	(all-syms (get-all-symbols grammar))
	(productions (get-production-list grammar)))

    ;; THE ALGORITHM AT LAST
    (let ((nullable (create-alist all-syms nil))
	  (first (create-alist all-syms nil))
	  (follow (create-alist all-syms nil)))

      (mapcar #'(lambda (ter) (add-alist-data-atom first ter ter))
	      terms)

      (until-no-change-at (nullable first follow)
	  #'copy-tree #'alist-of-set-equal
	(print-sets nullable first follow)
	(mapcar
	 #'(lambda (p)
	     (let ((lhs (car p))
		   (rhs (cdr p)))
	       
	       (if (or (eql rhs nil) (all-nullable-p rhs nullable))
		   (set-alist-data nullable (car p) t))
	       
	       (mapcar
		#'(lambda (s)
		    (union-alist-data first lhs
				      first s))
		(segments-from-case-1 rhs nullable))
	       
	       (mapcar
		#'(lambda (s)
		    (union-alist-data follow s
				      follow lhs))
		(segments-from-case-2 rhs nullable))
	       
	       (mapcar
		#'(lambda (p)
		    (union-alist-data follow (car p)
				      first (cdr p)))
		(segments-from-case-3 rhs nullable))))
	 productions))
      
      (list :nullable nullable
	    :first first
	    :follow follow))))
