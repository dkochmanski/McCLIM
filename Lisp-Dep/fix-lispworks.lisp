;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *packages-for-warn-on-redefinition*
        (remove-if 
         (lambda (p) (member p '("COMMON-LISP"
                                 "CLIM"
                                 "CLIM-INTERNALS"
                                 "CLIM-SYS"
                                 "CLIM-UTILS"
                                 "CLIM-LISP"
                                 "POSTSCRIPT-CLIM") :test #'equal))
         *packages-for-warn-on-redefinition*)))

(defpackage :clim-mop
  (:use :clos :common-lisp)
  (:export
   #:validate-superclass
   #:class-finalized-p
   #:finalize-inheritance
   #:class-prototype
   #:class-precedence-list
   #:class-direct-superclasses
   #:ensure-class
   #:funcallable-standard-class
   #:method-specializers
   #:generic-function-methods
   #:eql-specializer
   #:eql-specializer-object
   #:intern-eql-specializer
   #:compute-applicable-methods-using-classes))

(in-package :clim-mop)



(defmethod compute-applicable-methods-using-classes (gf classes)
  (values nil nil))

(ignore-errors
  (defmethod validate-superclass (a b)
    T))

(ignore-errors
(defmethod class-finalized-p (c)
  T))

(ignore-errors
  (defmethod finalize-inheritance (c)
    (values)))

(deftype eql-specializer () '(satisfies eql-specializer-p))

(defun eql-specializer-object (spec)
  (cadr spec))

;;; Pretty bogus, but should suit our purposes, whatever they are.

(defparameter *eql-specializer-hash* (make-hash-table))

(defun intern-eql-specializer (object)
  (let ((eql-object (gethash object *eql-specializer-hash* nil)))
    (unless eql-object
      (setq eql-object (cons 'eql object))
      (setf (gethash object *eql-specializer-hash*) eql-object))
    eql-object))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (sym :clos)
	(export sym :clim-mop)))

(defmethod clim-mop:class-direct-superclasses ((instance (eql *ptype-t-class*)))
  (list (find-class 'standard-object)))  ;; scary..
