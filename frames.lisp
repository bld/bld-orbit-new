(in-package :bld-orbit-new)

(defparameter *J2000* (list (ve3 :e1 1d0)
			    (ve3 :e2 1d0)
			    (ve3 :e3 1d0)) "J2000 inertial frame")

;;; Frame functions 

(defun rv-frame (x)
  "Form a reference frame from the position and velocity vectors to
CB. First is the unit position vector. Second is the complement of the
first in the orbit plane. Third (if it exists) is the orbit plane
normal vector."
  (list (ru x)
	(unitg (*i (r x) (h x)))
	(dual (h x))))

(defun orbit-frame (x)
  "Produce orbit frame from vector to sun"
  (with-derived (rsc-sun) tm x sc
    (with-slots (iframe) sc
      (let* ((o3 (unitg rsc-sun))
	     (o1 (unitg (*i (third iframe) (dual o3))))
	     (o2 (*i o3 (dual o1))))
	(list o1 o2 o3)))))
