
(defsystem #:clim-lisp
  :serial t
  :depends-on (#:closer-mop #:trivial-gray-streams)
  :components (;; First possible patches
               (:file "patch")
               (:module "Lisp-Dep"
                :components ((:file "fix-mop")))
               (:file "package")))
