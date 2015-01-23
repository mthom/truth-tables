(defun solver-form (symbols prop)
  (let ((p (gensym))
	(q (gensym)))   
    `(lambda (,@symbols)
       (declare (ignorable ,@symbols))
       (labels ((-> (,p ,q)
		  (or (not ,p) ,q))
		(xor (,p ,q)
		  (or (and ,p (not ,q)) (and (not ,p) ,q)))
		(<-> (,p ,q)
		  (and (-> ,p ,q) (-> ,q ,p))))
	 ,prop))))

(defun make-solvers (symbols props)
  (mapcar (lambda (prop)
	    (eval (solver-form symbols prop)))
	  props))

(defun apply-solvers (solvers assignments)
  (mapcar (lambda (solver)
	    (apply solver assignments))
	  solvers))

(defun operatorp (op)
  (member op '(and not or xor -> <->)))

(defun tree-walk (tree fun)
  (subst-if t
	    (constantly nil)
	    tree
	    :key fun))

(defun get-subforms (prop)
  (let (symbols compounds)
    (tree-walk prop
	       (lambda (form)
		 (when form
		       (cond ((and (consp form) (operatorp (first form)))
			      (push form compounds))
			     ((and (symbolp form) (not (operatorp form)))
			      (push form symbols))))))
    (values (remove-duplicates (nreverse symbols) :test #'equal)
	    (remove-duplicates compounds :test #'equal))))

(defun bitlist (value &optional (size 8)) 
  (map 'list
       (lambda (c)
	 (ecase c
	   (#\0 nil)
	   (#\1 t)))
       (format nil "~v,'0B" size value)))

(defun print-title (stream symbols subforms)
  (format stream "|~{ ~A |~}~%" (append symbols subforms)))

(defun print-row (stream assignments solutions subforms)
  (flet ((get-width (form)
	   (length (format nil "~A" form))))
    (let ((width-data (mapcar (lambda (solution form)
				(list (1- (get-width form)) solution))
			      solutions
			      subforms)))
      (format stream "|~{ ~:[F~;T~] |~}"   assignments)
      (format stream "~{ ~{~V,@T~:[F~;T~]~} |~}~%" width-data))))

(defun print-truth-table (prop &optional (stream t))
  (multiple-value-bind (symbols subforms)
      (get-subforms prop)
    (let ((n (length symbols))
	  (solvers (make-solvers symbols subforms)))
      (print-title stream symbols subforms)
      (loop for i downfrom (1- (ash 1 n)) to 0 do
	   (let* ((assignments (bitlist i n))
		  (solutions (apply-solvers solvers assignments)))
	     (print-row stream assignments solutions subforms))))))
