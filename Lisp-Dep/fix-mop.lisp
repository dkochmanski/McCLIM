(in-package :cl-user)

(defpackage #:clim-mop
  (:use #:c2mop)
  (:import-from #:c2mop #:method-qualifiers #:class-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
     do (export sym :clim-mop)))
