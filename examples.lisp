(in-package :bld-orbit-new)

;;; Ephemeris data

(setq *ephemeris-dir* (merge-pathnames-as-directory (pathname-as-directory (uiop:getenv "HOME")) (pathname-as-directory "src/ephemeris")))

;;; Solar system bodies

(defparameter *ssb*
  (make-instance 'body
		 :ephemeris "de430.bsp"
		 :name :ssb
		 :center :ssb
		 :ref :eclipj2000
		 :abcorr :none
		 :mu (with-ephemeris "gm_de431.tpc"
		       (body-vrd :sun :gm 1))))

(defparameter *sun*
  (make-instance 'body
		 :ephemeris (ephemeris-path "de430.bsp")
		 :name :sun
		 :center :ssb
		 :ref :eclipj2000
		 :abcorr :none
		 :mu (with-ephemeris "gm_de431.tpc"
		       (body-vrd :sun :gm 1))
		 :ls 1361.3477d0 ; solar luminosity in W/m^2 (same with km)
		 :ls-du *au*))

(defparameter *earth*
  (make-instance 'body
		 :ephemeris "de430.bsp"
		 :name :earth
		 :center :ssb
		 :ref :eclipj2000
		 :abcorr :none
		 :mu (with-ephemeris "gm_de431.tpc"
		       (body-vrd :earth :gm 1))))

(defparameter *mars*
  (make-instance 'body
		 :ephemeris "de430.bsp"
		 :name "MARS BARYCENTER"
		 :center :ssb
		 :ref :eclipj2000
		 :abcorr :none
		 :mu (with-ephemeris "gm_de431.tpc"
		       (body-vrd :mars :gm 1))))

;;; Cartesian

(defparameter *eg-cart-fixed*
  (make-hash*
   :cb *ssb*
   :mu (slot-value cb 'mu)
   :sun *sun*
   :ls (slot-value sun 'ls)
   :ls-du (slot-value sun 'ls-du)
   :mass 52d0
   :area (convert-unit '(1260d0 m^2) 'km^2)
   :iframe *j2000*
   :nfun #'first
   :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2))))
   :t0 (coerce (encode-universal-time 0 0 0 27 2 2014 0) 'double-float)
   :tf (coerce (encode-universal-time 0 0 0 27 2 2015 0) 'double-float)
   :x0 nil
   :hmin (/ (- tf t0) 2000d0)
   :hmax (- tf t0)
   :eom #'eom
   :tol 1d-9))
(let ((p *eg-cart-fixed*))
  (lethash (t0) p
    (setf (gethash :x0 p)
	  (with-slots (r v) (position-velocity *earth* t0)
	    (make-instance 'cart-state :r r :v v :problem p)))))

(defparameter *eg-cart-table*
  (make-hash*
   :cb *ssb*
   :mu (slot-value cb 'mu)
   :sun *sun*
   :ls (slot-value sun 'ls)
   :ls-du (slot-value sun 'ls-du)
   :mass 52d0
   :area (convert-unit '(1260d0 m^2) 'km^2)
   :iframe *j2000*
   :nfun #'first
   :rs nil
   :t0 (coerce (encode-universal-time 0 0 0 27 2 2014 0) 'double-float)
   :tf (coerce (encode-universal-time 0 0 0 27 2 2015 0) 'double-float)
   :rs-table (list
	      (make-hash
	       :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :t0 t0
	       :tf (/ (+ t0 tf) 2))
	      (make-hash
	       :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :t0 (/ (+ t0 tf) 2)
	       :tf tf))
   :x0 nil
   :hmin (/ (- tf t0) 2000)
   :hmax (/ (- tf t0) 2)
   :eom #'eom
   :tol 1d-9))
(let ((p *eg-cart-table*))
  (lethash (t0) p
    (setf (gethash :x0 p)
	  (with-slots (r v) (position-velocity *earth* t0)
	    (make-instance 'cart-state :r r :v v :problem p)))))

;;; Spinor

(defparameter *eg-spinor-fixed*
  (make-hash*
   :cb *ssb*
   :mu (slot-value cb 'mu)
   :sun *sun*
   :ls (slot-value sun 'ls)
   :ls-du (slot-value sun 'ls-du)
   :mass 52d0
   :area (convert-unit '(1260d0 m^2) 'km^2)
   :iframe *j2000*
   :nfun #'first
   :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2))))
   :t0 (coerce (encode-universal-time 0 0 0 27 2 2014 0) 'double-float)
   :tf (coerce (encode-universal-time 0 0 0 27 2 2015 0) 'double-float)
   :s0 0d0
   :sf (/ pi 14d0)
   :x0-cart nil
   :x0 nil
   :hmin (/ (- sf s0) 2000d0)
   :hmax (- sf s0)
   :eom #'eom
   :tol 1d-9
   :stopfn #'(lambda (s x p) (tm x))
   :stopval tf
   :stoptest #'>=
   :stoptol 1d-6))
(let ((p *eg-spinor-fixed*))
  (lethash (t0) p
    (setf (gethash :x0-cart p)
	  (with-slots (r v) (position-velocity *earth* t0)
	    (make-instance 'cart-state :r r :v v)))
    (setf (gethash :x0 p)
	  (to-spinor t0 (gethash :x0-cart p) p))))

(defparameter *eg-spinor-table*
  (make-hash*
   :cb *ssb*
   :mu (slot-value cb 'mu)
   :sun *sun*
   :ls (slot-value sun 'ls)
   :ls-du (slot-value sun 'ls-du)
   :mass 52d0
   :area (convert-unit '(1260d0 m^2) 'km^2)
   :iframe *j2000*
   :nfun #'first
   :rs nil
   :t0 (coerce (encode-universal-time 0 0 0 27 2 2014 0) 'double-float)
   :tf (coerce (encode-universal-time 0 0 0 27 2 2015 0) 'double-float)
   :rs-table (list
	      (make-hash
	       :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :stopfn #'(lambda (s x p) (tm x))
	       :stopval (/ (+ t0 tf) 2)
	       :stoptest #'>=
	       :stoptol 1d-6)
	      (make-hash
	       :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :stopfn #'(lambda (s x p) (tm x))
	       :stopval tf
	       :stoptest #'>=
	       :stoptol 1d-6))
   :s0 0d0
   :dsf (/ pi 14d0)
   :x0-cart nil
   :x0 nil
   :hmin (/ dsf 2000d0)
   :hmax (/ dsf 2)
   :eom #'eom
   :tol 1d-9
   :stopfn nil
   :stopval nil
   :stoptest nil
   :stoptol nil))
(let ((p *eg-spinor-table*))
  (lethash (t0) p
    (setf (gethash :x0-cart p)
	  (with-slots (r v) (position-velocity *earth* t0)
	    (make-instance 'cart-state :r r :v v)))
    (setf (gethash :x0 p)
	  (to-spinor t0 (gethash :x0-cart p) p))))

;;; Kustaanheimo-Stiefel

(defparameter *eg-ks-fixed*
  (make-hash*
   :cb *ssb*
   :mu (slot-value cb 'mu)
   :sun *sun*
   :ls (slot-value sun 'ls)
   :ls-du (slot-value sun 'ls-du)
   :mass 52d0
   :area (convert-unit '(1260d0 m^2) 'km^2)
   :iframe *j2000*
   :nfun #'first
   :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2))))
   :t0 (coerce (encode-universal-time 0 0 0 27 2 2014 0) 'double-float)
   :tf (coerce (encode-universal-time 0 0 0 27 2 2015 0) 'double-float)
   :s0 0d0
   :sf (/ pi 14d0)
   :x0-cart nil
   :x0-spin nil
   :x0 nil
   :hmin (/ (- sf s0) 2000d0)
   :hmax (- sf s0)
   :eom #'eom
   :tol 1d-9
   :stopfn #'(lambda (s x p) (tm x))
   :stopval tf
   :stoptest #'>
   :stoptol 1d-6))
(let ((p *eg-ks-fixed*))
  (lethash (t0 s0) p
    (setf (gethash :x0-cart p)
	  (with-slots (r v) (position-velocity *earth* t0)
	    (make-instance 'cart-state :r r :v v)))
    (setf (gethash :x0-spin p) 
	  (to-spinor t0 (gethash :x0-cart p) p))
    (setf (gethash :x0 p)
	  (to-initial-ks s0 (gethash :x0-spin p) p))))

(defparameter *eg-ks-table*
  (make-hash*
   :cb *ssb*
   :mu (slot-value cb 'mu)
   :sun *sun*
   :ls (slot-value sun 'ls)
   :ls-du (slot-value sun 'ls-du)
   :mass 52d0
   :area (convert-unit '(1260d0 m^2) 'km^2)
   :iframe *j2000*
   :nfun #'first
   :rs nil
   :t0 (coerce (encode-universal-time 0 0 0 27 2 2014 0) 'double-float)
   :tf (coerce (encode-universal-time 0 0 0 27 2 2015 0) 'double-float)
   :rs-table (list
	      (make-hash
	       :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :stopfn #'(lambda (s x p) (tm x))
	       :stopval (/ (+ t0 tf) 2)
	       :stoptest #'>=
	       :stoptol 1d-6)
	      (make-hash
	       :rs (rotor (make-instance 'bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :stopfn #'(lambda (s x p) (tm x))
	       :stopval tf
	       :stoptest #'>=
	       :stoptol 1d-6))
   :s0 0d0
   :dsf (/ pi 14d0)
   :x0-cart nil
   :x0-spin nil
   :x0 nil
   :hmin (/ dsf 2000d0)
   :hmax (/ dsf 2)
   :eom #'eom
   :tol 1d-9
   :stopfn nil
   :stopval nil
   :stoptest nil
   :stoptol nil))
(let ((p *eg-ks-table*))
  (lethash (t0 s0) p
    (setf (gethash :x0-cart p)
	  (position-velocity *earth* t0))
    (setf (gethash :x0-spin p)
	  (to-spinor t0 (gethash :x0-cart p) p))
    (setf (gethash :x0 p)
	  (to-initial-ks s0 (gethash :x0-spin p) p))))

;;; KS NSGA2 Earth to Mars transfer

(defun euler-rotation (angles sequence ref)
  (loop for a in angles
     for s in sequence
     for ssign = (signum s)
     for r = (rotor (* (- ssign) (dual (nth (1- (abs s)) ref))) a)
     then (rotor (* (- ssign) (dual (nth (1- (abs s)) f))) a)
     for f = (new-frame r ref) then (new-frame r f)
     for rs = r then (*g r rs)
     finally (return (values f rs))))

(defun ks-earth-mars-objfun (ind)
  (with-slots (xvar) ind
    (let* ((nvar 11)
	   (t0 (coerce (encode-universal-time 0 0 0 27 2 2014 0) 'double-float))
	   (n-rs (/ (1- nvar) 2))
	   (dtf (first xvar))
	   (tf (+ t0 dtf))
	   (clocks (subseq xvar 1 (1+ n-rs)))
	   (cones (subseq xvar (1+ n-rs) (length xvar)))
	   (rs-table
	    (loop with dtm = (/ (- tf t0) n-rs)
	       for i below n-rs
	       for tm = t0 then (+ tm dtm)
	       for tfi = (+ tm dtm)
	       for clock in clocks
	       for cone in cones
	       for rs = (nth-value 1 (euler-rotation (list clock cone) '(1 3) *j2000*))
	       collect (make-hash
			:rs rs
			:stopfn #'(lambda (s x p) (tm x))
			:stopval tfi
			:stoptest #'>=
			:stoptol 1d-6)))
	   (problem
	    (make-hash*
	     :cb *ssb*
	     :mu (slot-value cb 'mu)
	     :sun *sun*
	     :ls (slot-value sun 'ls)
	     :ls-du (slot-value sun 'ls-du)
	     :mass 52d0
	     :area (convert-unit '(1260d0 m^2) 'km^2)
	     :iframe *j2000*
	     :nfun #'first
	     :rs nil
	     :t0 t0
	     :tf tf
	     :rs-table rs-table
	     :s0 0d0
	     :dsf (/ pi 14)
	     :x0-cart nil
	     :x0-spin nil
	     :x0 nil
	     :hmin (/ dsf 20000)
	     :hmax (/ dsf 20)
	     :eom #'eom
	     :tol 1d-9
	     :stopfn nil
	     :stopval nil
	     :stoptest nil
	     :stoptol nil)))
      (setf (gethash :x0-cart problem)
	    (position-velocity *earth* t0))
      (setf (gethash :x0-spin problem)
	    (to-spinor t0 (gethash :x0-cart problem) problem))
      (setf (gethash :x0 problem)
	    (to-initial-ks (gethash :s0 problem) (gethash :x0-spin problem) problem))
      (let* ((traj (propagate-ks-table problem))
	     (sf (first (car (last traj))))
	     (xf-ks (second (car (last traj))))
	     (xf-spinor (to-spinor sf xf-ks problem))
	     (xf-cart (to-cartesian sf xf-ks problem))
	     (xf-target (position-velocity *mars* tf))
	     (xf-target-spinor (to-spinor tf xf-target (make-hash :iframe *j2000*)))
	     (xferr (- xf-target-spinor xf-spinor)))
	(values
	 (list dtf (norminf (u xferr)) (norminf (duds xferr)))
	 nil ; no constraints
	 problem traj))))) ; for later evaluation of individuals

(defparameter *ks-earth-mars-options*
  (make-instance
   'options
   :popsize 100
   :ngen 100
   :nobj 3
   :ncon 0
   :nvar (+ 1 5 5)
   :minvar (cons (convert-unit '(2 year) 'sec) (make-list 10 :initial-element (- (/ pi 2d0))))
   :maxvar (cons (convert-unit '(5 years) 'sec) (make-list 10 :initial-element (/ pi 2d0)))
   :pcross 0.9d0
   :pmut 0.5d0
   :eta-c 10d0
   :eta-m 20d0
   :objfun #'ks-earth-mars-objfun
   :parallel t))
