(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clim-lisp-patch)
    (make-package :clim-lisp-patch :use nil)))

;;; hack for multiple evaluation of DEFCONSTANT forms
(defmacro clim-lisp-patch::defconstant (symbol value &optional docu)
  `(defvar ,symbol ,value ,@(and docu (list docu))))

(export '(clim-lisp-patch::describe 
          clim-lisp-patch::describe-object 
          clim-lisp-patch::interactive-stream-p
          clim-lisp-patch::defconstant)
        :clim-lisp-patch)
