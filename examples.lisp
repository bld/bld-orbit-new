(in-package :bld-orbit-new)

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
		       (body-vrd :sun :gm 1))))

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
   :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2))))
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
	       :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :t0 t0
	       :tf (/ (+ t0 tf) 2))
	      (make-hash
	       :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
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
   :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2))))
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
   :stoptest #'>))
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
	       :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :stopfn #'(lambda (s x p) (tm x))
	       :stopval (/ (+ t0 tf) 2)
	       :stoptest #'>)
	      (make-hash
	       :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :stopfn #'(lambda (s x p) (tm x))
	       :stopval tf
	       :stoptest #'>))
   :s0 0d0
   :sf (/ pi 14d0)
   :x0-cart nil
   :x0 nil
   :hmin (/ (- sf s0) 2000d0)
   :hmax (/ (- sf s0) 2)
   :eom #'eom
   :tol 1d-9
   :stopfn nil
   :stopval nil
   :stoptest nil))
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
   :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2))))
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
   :stoptest #'>))
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
	       :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :stopfn #'(lambda (s x p) (tm x))
	       :stopval (/ (+ t0 tf) 2)
	       :stoptest #'>)
	      (make-hash
	       :rs (rotor (bve3 :e1e2 1) (atan (/ (sqrt 2d0))))
	       :stopfn #'(lambda (s x p) (tm x))
	       :stopval tf
	       :stoptest #'>))
   :s0 0d0
   :sf (/ pi 14d0)
   :x0-cart nil
   :x0-spin nil
   :x0 nil
   :hmin (/ (- sf s0) 2000d0)
   :hmax (/ (- sf s0) 2)
   :eom #'eom
   :tol 1d-9
   :stopfn nil
   :stopval nil
   :stoptest nil))
(let ((p *eg-ks-table*))
  (lethash (t0 s0) p
    (setf (gethash :x0-cart p)
	  (with-slots (r v) (position-velocity *earth* t0)
	    (make-instance 'cart-state :r r :v v)))
    (setf (gethash :x0-spin p) 
	  (to-spinor t0 (gethash :x0-cart p) p))
    (setf (gethash :x0 p)
	  (to-initial-ks s0 (gethash :x0-spin p) p))))
