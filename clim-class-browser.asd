(in-package :cl-user)

(defpackage :clim-class-browser.system
  (:use :cl :asdf))

(in-package :clim-class-browser.system)

(asdf:defsystem :clim-class-browser
    :serial t
    :description "Common Lisp CLOS class browser written in McCLIM"
    :author "Pocket7878 <poketo7878@gmail.com>"
    :depends-on (:closer-mop :mcclim :cl-annot)
    :components
    ((:file "package")
     (:file "clim-class-browser" :depends-on ("package")))
    )