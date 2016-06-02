(defpackage #:clim-mop
  (:use #:mop)
  (:shadowing-import-from #:pcl #:eql-specializer-object))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
	do (export sym :clim-mop))
  (loop for other-symbol in '("EQL-SPECIALIZER" "FUNCALLABLE-STANDARD-CLASS")
	unless (find-symbol other-symbol :clim-mop)
	do (let ((sym (intern other-symbol :pcl)))
	     (import sym :clim-mop)
	     (export sym :clim-mop))))


;;; In CMUCL the Common Lisp versions of class-of and find-class
;;; return wrappers which the MOP can't grok, so use the PCL versions
;;; instead.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((reexport (symbols)
           (import symbols :clim-lisp-patch)
           (export symbols :clim-lisp-patch)))
    (reexport '(pcl:class-name pcl:class-of
                pcl:find-class pcl::standard-class))))

