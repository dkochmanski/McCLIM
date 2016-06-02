(in-package :cl-user)

(eval-when (:compile-toplevel :execute)
  (when (find-package "SB-MOP")
    (pushnew :sb-mop *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:clim-mop)
    (make-package '#:clim-mop :use '(#+sb-mop #:sb-mop
                                     #-sb-mop #:sb-pcl))
    (shadowing-import 'sb-pcl::eql-specializer-object '#:clim-mop)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
     do (export sym :clim-mop))
  #-sb-mop
  (loop for other-symbol in '("EQL-SPECIALIZER" "FUNCALLABLE-STANDARD-CLASS")
     unless (find-symbol other-symbol :clim-mop)
     do (let ((sym (intern other-symbol :sb-pcl)))
          (import sym :clim-mop)
          (export sym :clim-mop))))
