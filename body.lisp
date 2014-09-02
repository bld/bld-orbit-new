(in-package :bld-orbit-new)

(defclass body ()
  ((ephemeris :initarg :ephemeris)
   (name :initarg :name)
   (ref :initarg :ref)
   (abcorr :initarg :abcorr)
   (center :initarg :center)
   (mu :initarg :mu)
   (ls :initarg :ls :documentation "Solar luminosity")
   (ls-du :initarg :ls-du :documentation "Distance unit where LS defined")))

(defparameter *j2000-utc-seconds*
  (+ (encode-universal-time 55 58 11 1 1 2000 0) 0.816d0))

(defun utcsec-to-ephemeris-time (utcsec)
  (- utcsec *j2000-utc-seconds*))

(defmethod position-velocity ((b body) teph)
  "Position & velocity of body"
  (with-slots (ephemeris name ref abcorr center) b
    (with-kernel ephemeris
      (let ((pvv (spk-ezr name (utcsec-to-ephemeris-time teph) center :ref ref :abcorr abcorr)))
	(make-instance 
	 'cart-state
	 :r (ve3 :e1 (aref pvv 0) :e2 (aref pvv 1) :e3 (aref pvv 2))
	 :v (ve3 :e1 (aref pvv 3) :e2 (aref pvv 4) :e3 (aref pvv 5)))))))

(defmethod body-position ((b body) teph)
  "Position of body relative to its center"
  (with-slots (name ref abcorr center) b
    (let ((pv (spk-pos name (utcsec-to-ephemeris-time teph) center :ref ref :abcorr abcorr)))
      (ve3 :e1 (aref pv 0) :e2 (aref pv 1) :e3 (aref pv 2)))))

