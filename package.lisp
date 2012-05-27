(in-package :cl-user)

(defpackage :clim-class-browser
  (:use :clim-lisp :clim-extensions :clim :cl-annot :clim-tab-layout)
  (:shadowing-import-from :closer-mop
			  :defclass
			  :defmethod
			  :standard-class
			  :ensure-generic-function
			  :defgeneric
			  :standard-generic-function
			  :class-name))

