(in-package :bld-orbit-new)

(def-sc-state ks-state ()
  :ivar s
  :pvar problem
  :problem-keys (cb mu sun ls ls-du mass area iframe nfun rs)
  :states
  ((alpha :initarg :alpha :accessor alpha :documentation "U at s=0")
   (beta :initarg :beta :accessor beta :documentation "dU/ds / w0 at s=0")
   (e :initarg :e :accessor e :documentation "Keplerian specific orbital energy")
   (tm :initarg :tm :accessor tm :documentation "Time")
   ;; Constants
   (w0 :initarg :w0 :accessor w0 :documentation "Initial average angular velocity")
   (hk0 :initarg :hk0 :accessor hk0 :documentation "Initial (- e)"))
  :derived
  ;; Same as spinor-state
  ((r-cb (body-position cb tm))
   (r-sun (body-position sun tm))
   (rm (norme2 u))
   ;; (e (/ (- (* 2d0 (norme2 duds)) mu) rm))
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
   (a (/ (* 2 p area (expt (scalar (*i ru-sc-sun n)) 2) n) mass))
   ;; Additional KS specific
   (hk (- e))
   (w (/ (- hk hk0) 2))
   (u (+ (* alpha (cos (* w0 s)))
	 (* beta (sin (* w0 s)))))
   (duds (* w0 (- (* beta (cos (* w0 s)))
		  (* alpha (sin (* w0 s))))))
   (ff (- (/ (*g a r u) 2) (* w u))))
  :options
  ((:documentation "Kustaanheimo-Stiefel orbital element state")))

(defstatearithmetic ks-state (alpha beta e tm) :oslots (w0 hk0 problem))

(defmethod print-object ((x ks-state) stream)
  (format stream "#<KS-STATE :ALPHA ~a :BETA ~a :E ~a :TM ~a>"
	  (alpha x) (beta x) (e x) (tm x)))

(defmethod eom (s (x ks-state) problem)
  (setf (s x) s)
  (with-accessors ((ff ff) (rm rm) (v v) (a a) (w0 w0) (hk0 hk0)) x
    (make-instance
     'ks-state
     :alpha (- (* ff (/ (sin (* w0 s)) w0)))
     :beta (* ff (/ (cos (* w0 s)) w0))
     :e (* rm (scalar (*i v a)))
     :tm rm
     :w0 w0
     :hk0 hk0
     :problem problem)))

(defmethod to-spinor (s (x ks-state) problem)
  "Convert KS state to spinor given S, X, and PROBLEM"
  (setf (s x) s)
  (values
   (make-instance
    'spinor-state
    :u (u x)
    :duds (duds x)
    :tm (tm x)
    :problem problem)
   s))

(defmethod to-cartesian (s (x ks-state) problem)
  (to-cartesian s (to-spinor s x problem) problem))

(defmethod to-initial-ks (s (x0 spinor-state) problem)
  "Convert spinor state to an initial KS given S, X, and PROBLEM"
  (setf (s x0) s)
  (with-accessors ((u u) (duds duds) (tm tm) (e e)) x0
    (let* ((hk0 (- e))
	   (w0 (sqrt (/ hk0 2))))
      (make-instance
       'ks-state
       :alpha (- (* u (cos (* w0 s)))
		 (* (/ duds w0) (sin (* w0 s))))
       :beta (+ (* u (sin (* w0 s)))
		(* (/ duds w0) (cos (* w0 s))))
       :e e
       :tm tm
       :hk0 hk0
       :w0 w0
       :problem problem))))

(defmethod to-initial-ks (tm (x0 cart-state) problem)
  (let ((x0-spin (to-spinor tm x0 problem)))
    (to-initial-ks 0d0 x0-spin problem)))

(defun propagate-ks-fixed (problem)
  (lethash (eom s0 sf x0 hmin hmax cb sun stopfn stopval stoptest stoptol) problem
    (with-kernel (ephemeris-path (slot-value cb 'ephemeris))
      (with-kernel (ephemeris-path (slot-value sun 'ephemeris))
	(rka-stop-nr 
	 eom s0 sf x0
	  :tol 1d-9 :param problem :hmin hmin :hmax hmax
	  :stopfn stopfn :stopval stopval :stoptest stoptest :stoptol stoptol)))))

(defun propagate-ks-table (p)
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

(defun export-ks-traj (filename result problem)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-csv
     (loop for (s x) in result
	for (xc tm) = (multiple-value-list (to-cartesian s x problem))
	collect (cons tm
		      (append
		       (map 'list #'identity (coef (r xc)))
		       (map 'list #'identity (coef (v xc))))))
     :stream stream :separator #\Space)))
