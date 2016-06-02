(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clim-lisp-patch)
    (make-package :clim-lisp-patch :use nil)))

;;; hack for multiple evaluation of DEFCONSTANT forms
(defmacro clim-lisp-patch::defconstant (symbol value &optional docu)
  `(defvar ,symbol ,value ,@(and docu (list docu))))

;;; hack for recording clos classes from the compile time
(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(defmacro clim-lisp-patch::defclass (name &rest args)
  `(progn
     (eval-when (:compile-toplevel)
       (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:defclass ,name ,@args))))

(export '(clim-lisp-patch::describe
          clim-lisp-patch::describe-object
          clim-lisp-patch::interactive-stream-p
          clim-lisp-patch::defconstant
          clim-lisp-patch::defclass)
        :clim-lisp-patch)
