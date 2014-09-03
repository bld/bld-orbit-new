(asdf:defsystem :bld-orbit-new
  :author "Ben Diedrich"
  :license "MIT"
  :description "Orbital mechanics library"
  :depends-on 
  ("bld-ga" "bld-e2" "bld-e3" "bld-ode" "bld-utils" "bld-gen" "local-time" "cl-spice" "cl-nsga2" "cells" "unit-formulas" "alexandria" "cl-fad")
  :serial t
  :components
  ((:file "package")
   (:file "constants")
   (:file "point")
   (:file "state")
   (:file "body")
   (:file "cartesian")
   (:file "frames")
   (:file "spinor")
   (:file "ks")
   (:file "examples")))
