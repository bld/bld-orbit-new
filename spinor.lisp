(in-package :bld-orbit-new)

(def-sc-state spinor-state ()
  :ivar s
  :pvar problem
  :problem-keys (cb mu sun ls ls-du mass area iframe nfun rs)
  :states
  ((u :initarg :u :accessor u :documentation "Spinor of position: r = u e1 (revg u)")
   (duds :initarg :duds :accessor duds :documentation "Spinor derivative wrt s")
   (tm :initarg :tm :accessor tm :documentation "Time"))
  :derived
  ((r-cb (body-position cb tm))
   (r-sun (body-position sun tm))
   (rm (norme2 u))
   (e (/ (- (* 2d0 (norme2 duds)) mu) rm))
   (dudt (/ duds rm))
   (r (spin (first iframe) u))
   (ru (unitg r))
   (r-sc (+ r-cb r))
   (r-sc-sun (- r-sc r-sun))
   (ru-sc-sun (unitg r-sc-sun))
   (rm-sc-sun (norme r-sc-sun))
   (v (graden (* 2 (*g3 dudt (first iframe) (revg u))) 1))
   (h (unitg (*o r v)))
   (oframe (list ru (unitg (*i r h)) (dual h)))
   (rotor-o (recoverrotor oframe iframe))
   (rotor-p (*g rotor-o rs))
   (n (funcall nfun (new-frame rotor-p iframe)))
   (p (/ (* ls (expt (/ ls-du rm-sc-sun) 2)) *c*))
   (a (/ (* 2 p area (expt (scalar (*i ru-sc-sun n)) 2) n) mass)))
  :options
  ((:documentation "Spinor state")))

(defstatearithmetic spinor-state (u duds tm) :oslots (problem))

(defmethod print-object ((o spinor-state) stream)
  (format stream "#<SPINOR-STATE :U ~a :DUDS ~a :TM ~a>" (u o) (duds o) (tm o)))

(defmethod eom (s (x spinor-state) problem)
  (setf (s x) s)
  (make-instance
   'spinor-state
   :u (duds x)
   :duds (/ (+ (*g3 (a x) (r x) (u x)) (* (e x) (u x))) 2d0)
   :tm (rm x)
   :problem problem))

(defmethod to-spinor (tm (x cart-state) problem)
  (setf (tm x) tm)
  (lethash (iframe) problem
    (let ((u (recoverspinor (norme (r x)) (rv-frame x) iframe)))
      (make-instance
       'spinor-state
       :u u
       :duds (/ (*g3 (v x) u (first iframe)) 2d0)
       :tm tm
       :problem problem))))

(defmethod to-cartesian (s (x spinor-state) problem)
  (setf (s x) s)
  (values
   (make-instance
    'cart-state
    :r (r x)
    :v (v x)
    :problem problem)
   (tm x)))


(defun propagate-spinor-fixed (problem)
  (lethash (eom s0 sf x0 hmin hmax cb sun stopfn stopval stoptest stoptol) problem
    (with-kernel (ephemeris-path (slot-value cb 'ephemeris))
      (with-kernel (ephemeris-path (slot-value sun 'ephemeris))
	(rka-stop-nr
	 eom s0 sf x0
	 :tol 1d-9 :param problem :hmin hmin :hmax hmax
	 :stopfn stopfn :stopval stopval :stoptest stoptest :stoptol stoptol)))))

(defun propagate-spinor-table (p)
  (lethash (eom s0 dsf rs-table x0 hmin hmax cb sun) p
    (with-kernel (ephemeris-path (slot-value cb 'ephemeris))
      (with-kernel (ephemeris-path (slot-value sun 'ephemeris))
	(loop for rsi in rs-table
	   for x0i = x0 then (second (car (last result)))
	   for s0i = s0 then (first (car (last result)))
	   for sfi = (+ s0i dsf)
	   for stopfn = (setf (gethash :stopfn p) (gethash :stopfn rsi))
	   for stopval = (setf (gethash :stopval p) (gethash :stopval rsi))
	   for stoptest = (setf (gethash :stoptest p) (gethash :stoptest rsi))
	   for stoptol = (setf (gethash :stoptol p) (gethash :stoptol rsi))
	   for rs = (setf (gethash :rs p) (gethash :rs rsi))
	   for result =
	     (rka-stop-nr 
	      eom s0i sfi x0i
	      :tol 1d-9 :param p :hmin hmin :hmax hmax
	      :stopfn stopfn :stopval stopval :stoptest stoptest :stoptol stoptol)
	   append result)))))
