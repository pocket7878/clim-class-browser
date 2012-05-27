(in-package :clim-class-browser)

(enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun specializer-methods (class)
  (remove-duplicates 
   (loop for class in (c2mop:class-precedence-list class)
      append (c2mop:specializer-direct-methods class))
   :test #'equal :key (lambda (x) (c2mop:generic-function-name (c2mop:method-generic-function x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define presentation types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-presentation-type class-slot ())
(define-presentation-type class-direct-slot ()
  :inherit-from 'class-slot)
(define-presentation-type class-method ())
(define-presentation-type class-direct-method ()
  :inherit-from 'class-method)
(define-presentation-type clos-class ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define main frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-application-frame class-browser ()
  ((current-class :initform t :initarg :on-class :accessor current-class))
  (:pointer-documentation t)
  (:panes
   (direct-slots-pane
    :application
    :display-time nil
    :display-function 'display-direct-slots-pane)
   (all-slots-pane
    :application
    :display-time nil
    :display-function 'display-slots-pane)
   (methods-pane
    :application
    :display-time nil
    :display-function 'display-methods-pane)
   (all-methods-pane
    :application
    :display-time nil
    :display-function 'display-all-methods-pane)
   (supers-pane
    :application
    :display-time nil
    :display-function 'display-supers-pane)
   (subs-pane
    :application
    :display-time nil
    :display-function 'display-subs-pane)
   (precedence-pane
    :application
    :display-time nil
    :display-function 'display-precedence-pane)
   (int :interactor :height 100 :width 600))
  (:layouts
   (default
       (vertically ()
	 (with-tab-layout ('tab-page)
	   ("Direct Slots" (scrolling () direct-slots-pane))
	   ("All Slots " (scrolling () all-slots-pane))
	   ("Methods" (scrolling () methods-pane))
	   ("All Methods" (scrolling () all-methods-pane))
	   ("Supers" (scrolling () supers-pane))
	   ("Subs" (scrolling () subs-pane))
	   ("Precedence" (scrolling () precedence-pane)))
	 ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class-browser-command (com-quit :name t
					:keystroke (#\q :meta)) ()
    (frame-exit *application-frame*))

(define-class-browser-command (com-browse-class) ((class 'clos-class))
  (setf (current-class *application-frame*) class)
  (redisplay-frame-panes *application-frame* :force-p t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define Command translators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-presentation-to-command-translator
    browse-class
    (clos-class com-browse-class class-browser :gesture :inspect)
  (object)
  `(,object))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define display pane functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun display-direct-slots-pane (frame pane)
  (loop for slot in (c2mop:class-direct-slots
		 (current-class frame))
       do
       (with-output-as-presentation
	   (pane slot 'class-direct-slot)
	 (format pane "~A~%"(c2mop:slot-definition-name slot)))))

(defun display-slots-pane (frame pane)
  (loop for slot in (c2mop:class-slots
		 (current-class frame))
       do
       (with-output-as-presentation
	   (pane slot 'class-slot)
	 (format pane "~A~%"(c2mop:slot-definition-name slot)))))

(defun display-methods-pane (frame pane)
  (loop for method in (remove-duplicates
		   (c2mop:specializer-direct-methods
		    (current-class frame))
		   :test #'equal)
       do
       (with-output-as-presentation
	   (pane method 'class-direct-method)
	 (format pane "~A~%" (c2mop:generic-function-name
			      (c2mop:method-generic-function method))))
       ))

(defun display-all-methods-pane (frame pane)
  (loop for method in (remove-duplicates
		       (specializer-methods
			(current-class frame))
		       :test #'equal)
       do
       (with-output-as-presentation
	   (pane method 'class-direct-method)
	 (format pane "~A~%" (c2mop:generic-function-name
			      (c2mop:method-generic-function method))))
       ))

(defun display-supers-pane (frame pane)
  (loop for super in (c2mop:class-direct-superclasses
		      (current-class frame))
       do
       (with-output-as-presentation
	   (pane super 'clos-class)
	 (format pane "~A~%" (class-name super)))))

(defun display-subs-pane (frame pane)
  (loop for sub in (c2mop:class-direct-subclasses
		  (current-class frame))
       do
       (with-output-as-presentation
	   (pane sub 'clos-class)
	 (format pane "~A~%" (class-name sub)))))
(defun display-precedence-pane (frame pane)
  (loop for p in (c2mop:class-precedence-list
	      (current-class frame))
       do
       (with-output-as-presentation
	   (pane p 'clos-class)
	 (format pane "~A~%" (class-name p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defun browse-class (class-name)
  (let ((found-class (find-class class-name nil)))
    (when found-class
      (run-frame-top-level
       (make-application-frame 'class-browser
			       :on-class found-class)))))