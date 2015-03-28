(in-package :bld-orbit-new)

(defvar *ephemeris-dir*)

(defun ephemeris-path (ephemeris)
  "Generate full path to an ephemeris file in the directory specified
by environment variable EPHEMERIS_DIR"
  (namestring
   (merge-pathnames-as-file
    *ephemeris-dir*
    ephemeris)))

(defmacro with-ephemeris (ephemeris &body body)
  `(with-kernel (ephemeris-path ,ephemeris)
     ,@body))

(defclass body ()
  ((ephemeris :initarg :ephemeris)
   (name :initarg :name)
   (ref :initarg :ref)
   (abcorr :initarg :abcorr)
   (center :initarg :center)
   (mu :initarg :mu)
   (ls :initarg :ls :documentation "Solar luminosity")
   (ls-du :initarg :ls-du :documentation "Distance unit where LS defined")
   (iframe :initarg :iframe :initform *j2000* :documentation "List of basis vvectors for intertial frame")))

(defparameter *j2000-utc-seconds*
  (+ (encode-universal-time 55 58 11 1 1 2000 0) 0.816d0))

(defun utcsec-to-ephemeris-time (utcsec)
  (- utcsec *j2000-utc-seconds*))

(defmethod position-velocity ((b body) teph)
  "Position & velocity of body"
  (with-slots (ephemeris name ref abcorr center) b
    (with-ephemeris ephemeris
      (let ((pvv (spk-ezr name (utcsec-to-ephemeris-time teph) center :ref ref :abcorr abcorr)))
	(make-instance 
	 'cart-state
	 :r (make-instance 've3 :e1 (aref pvv 0) :e2 (aref pvv 1) :e3 (aref pvv 2))
	 :v (make-instance 've3 :e1 (aref pvv 3) :e2 (aref pvv 4) :e3 (aref pvv 5)))))))

(defmethod body-position ((b body) teph)
  "Position of body relative to its center"
  (with-slots (ephemeris name ref abcorr center) b
    (with-ephemeris ephemeris
      (let ((pv (spk-pos name (utcsec-to-ephemeris-time teph) center :ref ref :abcorr abcorr)))
	(make-instance 've3 :e1 (aref pv 0) :e2 (aref pv 1) :e3 (aref pv 2))))))

(defmethod body-trajectory (b t0 tf &key (nsteps 100))
  (with-ephemeris (slot-value b 'ephemeris)
    (loop for tm = t0 then (incf tm (/ (- tf t0) nsteps))
       collect (list tm (body-position b tm))
       while (< tm tf))))

(defmethod export-body-traj (filename traj)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-csv
     (loop for (tm r) in traj
	collect (cons tm (coerce (coef r) 'list)))
     :stream stream
     :separator #\space)))
     
