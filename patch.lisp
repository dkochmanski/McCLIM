(in-package :cl-user)

;;; ensure clim-lisp-patch package
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clim-lisp-patch)
    (make-package :clim-lisp-patch :use nil)))

;;; hack for multiple evaluation of DEFCONSTANT forms
(defmacro clim-lisp-patch::defconstant (symbol value &optional docu)
  `(defvar ,symbol ,value ,@(and docu (list docu))))

#| this needs to be tested with clisp (package-locks etc)
;;; fulfill CLIM expectations regarding the gray streams

;;; INPUT-STREAM-P
(unless (typep #'input-stream-p 'generic-function)
  (setf (fdefinition
         (intern "ORIGINAL-INPUT-STREAM-P"
                 (find-package :trivial-gray-streams)))
        #'input-stream-p)
  (fmakunbound 'input-stream-p)

  (defgeneric input-stream-p (stream)
    (:method ((stream stream))
      (funcall (fdefinition
                (intern "ORIGINAL-OUTPUT-STREAM-P"
                        (find-package :trivial-gray-streams))) stream))))

;;; OUTPUT-STREAM-P
(unless (typep #'output-stream-p 'generic-function)
  (setf (fdefinition
         (intern "ORIGINAL-OUTPUT-STREAM-P"
                 (find-package :trivial-gray-streams)))
        #'output-stream-p)
  (fmakunbound 'output-stream-p)

  (defgeneric output-stream-p (stream)
    (:method ((stream stream))
      (funcall (fdefinition
                (intern "ORIGINAL-OUTPUT-STREAM-P"
                        (find-package :trivial-gray-streams))) stream))))
|#

 ;; export symbols to patch
;;; hack for recording clos classes from the compile time
(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(export '(clim-lisp-patch::describe
          clim-lisp-patch::describe-object
          clim-lisp-patch::interactive-stream-p
          clim-lisp-patch::defconstant)
        :clim-lisp-patch)
