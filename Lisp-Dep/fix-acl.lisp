;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

;;; Needed to keep ACL from issuing warnings about toplevel (shadow ...) forms
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :loop)
  (require :mop))

(defpackage :clim-mop
    (:use :clos :common-lisp)
    (:export #:accessor-method-slot-definition
	     #:add-dependent
	     #:add-direct-method
	     #:add-direct-subclass
	     #:add-method
	     #:allocate-instance
	     #:built-in-class
	     #:class
	     #:class-default-initargs
	     #:class-direct-default-initargs
	     #:class-direct-slots
	     #:class-direct-subclasses
	     #:class-direct-superclasses
	     #:class-finalized-p
	     #:class-name
	     #:class-precedence-list
	     #:class-prototype
	     #:class-slots
	     #:compute-applicable-methods
	     #:compute-applicable-methods-using-classes
	     #:compute-class-precedence-list
	     #:compute-default-initargs
	     #:compute-discriminating-function
	     #:compute-effective-method
	     #:compute-effective-slot-definition
	     #:compute-slots
	     #:direct-slot-definition
	     #:direct-slot-definition-class
	     #:effective-slot-definition
	     #:effective-slot-definition-class
	     #:ensure-class
	     #:ensure-class-using-class
	     #:ensure-generic-function
	     #:ensure-generic-function-using-class
	     #:eql-specializer
	     #:eql-specializer-object
	     #:extract-lambda-list
	     #:extract-specializer-names
	     #:finalize-inheritance
	     #:find-method-combination
	     #:forward-referenced-class
	     #:funcallable-standard-class
	     #:funcallable-standard-instance-access
	     #:funcallable-standard-object
	     #:function
	     #:generic-function
	     #:generic-function-argument-precedence-order
	     #:generic-function-declarations
	     #:generic-function-lambda-list
	     #:generic-function-method-class
	     #:generic-function-method-combination
	     #:generic-function-methods
	     #:generic-function-name
	     #:intern-eql-specializer
	     #:make-instance
	     #:make-method-lambda
	     #:map-dependents
	     #:metaobject
	     #:method
	     #:method-combination
	     #:method-function
	     #:method-generic-function
	     #:method-lambda-list
	     #:method-qualifiers
	     #:method-specializers
	     #:reader-method-class
	     #:remove-dependent
	     #:remove-direct-method
	     #:remove-direct-subclass
	     #:remove-method
	     #:set-funcallable-instance-function
	     #:slot-boundp-using-class
	     #:slot-definition
	     #:slot-definition-allocation
	     #:slot-definition-initargs
	     #:slot-definition-initform
	     #:slot-definition-initfunction
	     #:slot-definition-location
	     #:slot-definition-name
	     #:slot-definition-readers
	     #:slot-definition-type
	     #:slot-definition-writers
	     #:slot-makunbound-using-class
	     #:slot-value-using-class
	     #:specializer
	     #:specializer-direct-generic-functions
	     #:specializer-direct-methods
	     #:standard-accessor-method
	     #:standard-class
	     #:standard-direct-slot-definition
	     #:standard-effective-slot-definition
	     #:standard-generic-function
	     #:standard-instance-access
	     #:standard-method
	     #:standard-object
	     #:standard-reader-method
	     #:standard-slot-definition
	     #:standard-writer-method
	     #:update-dependent
	     #:validate-superclass
	     #:writer-method-class))




