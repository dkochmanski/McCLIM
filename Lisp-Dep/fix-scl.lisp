;;;; Support for the Scieneer Common Lisp.

(defpackage :clim-mop
  (:use :clos))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
	do (export sym :clim-mop)))

