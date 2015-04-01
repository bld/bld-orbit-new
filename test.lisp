;;;; Compare results of integration with GMAT R2013a

(ql:quickload :bld-orbit-new)

(in-package :bld-orbit-new)

(defparameter *gmat-r2013a-spk* (merge-pathnames-as-directory (user-homedir-pathname)  #p"bin/GMAT_2013a/data/planetary_ephem/spk/"))

(setq *ephemeris-dir* *gmat-r2013a-spk*)

(defparameter *gmat-de421* "DE421AllPlanets.bsp")

;;; Initial condition
(defparameter *gmat-t0* *j2000-utc-seconds* "GMAT inital time in UTC")

(defparameter *gmat-tf* (+ *gmat-t0* (* 365.25 (convert-unit 'days 'seconds))))

(defparameter *gmat-sun*
  (make-instance
   'body
   :ephemeris *gmat-de421*
   :name :sun
   :ref :eclipj2000
   :abcorr :none
   :center :ssb
   :mu 132712440017.99d0)
  "Sun from GMAT problem")

(defparameter *gmat-earth*
  (make-instance
   'body
   :ephemeris *gmat-de421*
   :name :earth
   :ref :eclipj2000
   :abcorr :none
   :center :ssb
   :mu 398600.4415d0)
  "Earth from GMAT problem")

(defparameter *gmat-test01*
  (make-hash*
   :cb *gmat-sun*
   :mu (slot-value cb 'mu)
   :t0 *gmat-t0*
   :tf *gmat-tf*
   :x0 nil
   :hmin (/ (- tf t0) 2000d0)
   :hmax (- tf t0)
   :eom #'eom
   :tol 1d-11)
  "Test problem to compare to that in GMAT")
(let ((p *gmat-test01*))
  (lethash (t0) p
    (setf (gethash :x0 p)
	  (with-slots (r v) (position-velocity *gmat-earth* t0)
	    (make-instance 'cart-kepler-state :r r :v v :problem p)))))

