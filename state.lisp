(in-package :bld-orbit-new)

(defmethod norminfx ((x g))
  "Infinity norm of geometric algebra state variable. Used in state equations of motion."
  (norminf x))

(defun walk-tree-atoms (fun tree)
  (tree-equal tree tree
              :test (lambda (element-1 element-2)
                      (declare (ignore element-2))
                      (funcall fun element-1)
                      t)))

(defmacro def-sc-state (name direct-superclass &key (ivar 'tm) (pvar 'problem) states derived problem-keys options)
  "Define a spacecraft state"
  (let* ((svars (mapcar #'first states))
	 (dvars (mapcar #'first derived))
	 (vars (cons ivar (append svars dvars)))
	 (dforms (mapcar #'second derived)))
    (dolist (var vars)
      (setq dforms (subst `(,(build-symbol ^ (:< var))) var dforms)))
    `(defmodel ,name ,direct-superclass
       (,@(loop for spec in states
	     collect `(,@spec))
	  (,ivar :accessor ,ivar :initarg ,(make-keyword ivar) :initform (c-in 0d0) :documentation "Integration variable")
	  (,pvar :accessor ,pvar :initarg ,(make-keyword pvar) :initform nil :documentation "Problem data")
	  ,@(loop for slot in dvars
	       for form in dforms
	       for lethash-keys =
		 (let (keys)
		   (walk-tree-atoms
		    #'(lambda (e) (when (find e problem-keys)
				    (pushnew e keys)))
		    form)
		   keys)
	       for pvar-cell = `(,(build-symbol ^ (:< pvar)))
	       collect `(,slot :accessor ,slot
			       :initform (c?_ (lethash ,lethash-keys ,pvar-cell ,form)))))
       ,@options)))
