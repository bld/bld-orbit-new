(in-package :bld-orbit-new)

(def-sc-state cart-kepler-state ()
  :ivar tm
  :pvar problem
  :problem-keys (cb mu t0 tf x0 hmin hmax eom tol)
  :states
  ((r :initarg :r :accessor r :initform (ve3) :documentation "Position vector")
   (v :initarg :v :accessor v :initform (ve3) :documentation "Velocity vector"))
  :derived
  ((r-cb (body-position cb tm))
   (r-sc (+ r-cb r))
   (ru (unitg r))
   (rm2 (norme2 r))
   (g (- (* (/ mu rm2) ru))))
  :options
  ((:documentation "Cartesian Kepler orbit state")))

(defmethod print-object ((x cart-kepler-state) stream)
  (format stream "#<CART-KEPLER-STATE :r ~a :v ~a>" (r x) (v x)))

(defstatearithmetic cart-kepler-state (r v) :oslots (problem))

(defmethod eom (tm (x cart-kepler-state) problem)
  (setf (tm x) tm)
  (make-instance 'cart-kepler-state :r (v x) :v (g x) :problem problem))

(defun propagate-cart-kepler (problem)
  (lethash (eom t0 tf x0 hmin hmax cb tol) problem
    (with-kernel (ephemeris-path (slot-value cb 'ephemeris))
      (rka eom t0 tf x0 :param problem :hmin hmin :hmax hmax :tol tol))))
   
(def-sc-state cart-state ()
  :ivar tm
  :pvar problem
  :problem-keys (cb mu sun ls ls-du mass area iframe nfun rs t0 tf x0 hmin hmax)
  :states
  ((r :initarg :r :accessor r :initform (ve3) :documentation "Position vector")
   (v :initarg :v :accessor v :initform (ve3) :documentation "Velocity vector"))
  :derived
  ((r-cb (body-position cb tm))
   (r-sc (+ r-cb r))
   (r-sun (body-position sun tm))
   (r-sc-sun (- r-sc r-sun))
   (ru-sc-sun (unitg r-sc-sun))
   (rm-sc-sun (norme r-sc-sun))
   (ru (unitg r))
   (rm2 (norme2 r))
   (h (unitg (*o r v)))
   (oframe (list ru (unitg (*i r h)) (dual h)))
   (rotor-o (recoverrotor oframe iframe))
   (rotor-p (*g rotor-o rs))
   (n (funcall nfun (new-frame rotor-p iframe)))
   (p (/ (* ls (expt (/ ls-du rm-sc-sun) 2)) *c*))
   (a (/ (* 2 p area (expt (scalar (*i ru-sc-sun n)) 2) n) mass))
   (g (- (* (/ mu rm2) ru))))
  :options
  ((:documentation "Cartesian state")))

(defmethod print-object ((x cart-state) stream)
  (format stream "#<CART-STATE :r ~a :v ~a>" (r x) (v x)))

(defstatearithmetic cart-state (r v) :oslots (problem))

(defmethod eom (tm (x cart-state) problem)
  (setf (tm x) tm)
  (make-instance 'cart-state :r (v x) :v (+ (a x) (g x)) :problem problem))

(defun propagate-cart-fixed (problem)
  (lethash (eom t0 tf x0 hmin hmax cb sun) problem
    (with-kernel (ephemeris-path (slot-value cb 'ephemeris))
      (with-kernel (ephemeris-path (slot-value sun 'ephemeris))
	(rka eom t0 tf x0 :param problem :hmin hmin :hmax hmax :tol 1d-9)))))

(defun propagate-cart-table (problem)
  (lethash (eom x0 t0 tf rs-table cb sun hmin hmax tol) problem
    (with-kernel (ephemeris-path (slot-value cb 'ephemeris))
      (with-kernel (ephemeris-path (slot-value sun 'ephemeris))
	(loop for rsi in rs-table
	   for x0i = x0
	   then (second (car (last result)))
	   for t0i = (gethash :t0 rsi)
	   for tfi = (gethash :tf rsi)
	   for rs = (setf (gethash :rs problem) (gethash :rs rsi))
	   for result = (rka eom t0i tfi x0i :param *eg-cart-fixed* :hmin hmin :hmax hmax :tol tol)
	   append result)))))
