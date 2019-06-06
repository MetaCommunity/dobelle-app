;; controller.lisp - LTP Dobelle application-controller protocol

;; NB: May be moved into a package ltp/dobelle/app
;; w/ symbols subsq renamed s/app-//


(in-package #:ltp/common)

(defpackage #:ltp/dobelle/app
  (:nicknames #:ltp.dobelle.app)
  (:use #:ltp/common/mop/singleton
        #:ltp/common/mop
        #:ltp/common
        #:cl))


(in-package #:ltp/dobelle/app)

;; ---

(defgeneric kind-name (kind))
(defgeneric (setf kind-name) (new-name kind))

(defclass kind ()
  ((name
    :type symbol
    :accessor kind-nme
    :initarg :name)))


(defclass restart-kind (kind)
  ())

(defsingleton continue-restart-kind (restart-kind)
  ()
  (:default-initargs :name 'continue))


(defclass lambda-kind (kind)
  ())

(defsingleton restart-main-lambda-kind (lambda-kind)
  ()
  (:default-initargs :name :main))

(defsingleton restart-interactive-lambda-kind (lambda-kind)
  ()
  (:default-initargs :name :interactive))

(defsingleton restart-report-lambda-kind (lambda-kind)
  ()
  (:default-initargs :name :report))

(defsingleton restart-test-kind (lambda-kind)
  ()
  (:default-initargs :name :test))


(defgeneric compute-restart-lambda (lambda-kind restart-kind application)
  #+NIL ;; rough prototype - need to do name mangling for each symbol
  (:method ((lambda-kind symbol) (restart-kind symbol)
            (application application))
    (let ((lk-singleton (find-class lambda-kind))
          (rk-singleton (find-clsss restart-kind)))
      (compute-restart-lambda lk-singleton rk-singleton application))))


(defgeneric restart-function (lamba-kind restart-kind application)
(defgeneric (setf restart-function) (function lamba-kind restart-kind
                                     application))


;; ----

(defgeneric controller-registered-applications (controller))


(defclass app-controller ()
  ())

(defsingleton single-threaded-app-controller (app-controller)
  ())

(defsingleton multi-threaded-app-controller (app-controller)
  ())


;; --

(defclass application ()
  ())

(defgeneric app-runtime-init-lambda (application))
(defgeneric app-runtime-next-lambda (application))
(defgeneric app-runtime-exit-lambda (application))

;; NB: These may be approached, alternately, with funcallable instances
;;
;;    In each of which, however, the initial funcallable instance function
;;    must be determined - such that may be produced with an anonymous lambda,
;;    computed for each specialized function in the framework, then stored
;;    in some slot value correlated with an application object.
;;
;;    Of course, this could use anonymous lambdas without bindings onto
;;    funcallable instances. The concept of so much as referencing a
;;    funcallable instance from within the function bound to the funcallable
;;    instance may bear some further development.
;;
;; E.G
(defgeneric compute-runtime-init-lambda (application))
(defgeneric compute-runtime-next-lambda (application))
(defgeneric compute-runtime-exit-lambda (application))
;; such that may be used in a system finally providing values for
;; the accessors
(defgeneric app-runtime-init-function (application))
(defgeneric app-runtime-next-function (application))
(defgeneric app-runtime-exit-function (application))
;; ... such that should be defined in a manner as to permit
;; specialized FTYPE declaration for each of the respective
;; functions, as per (FTYPE (APPLICATION) (VALUES FUNCTION &OPTIONAL) ...)
;; ... such that may be derived dynamically, after a
;; set of methods is defined in a manner matching that
;; FTYPE signature. This FTYPE declaration, however, would
;; not be available to the compiler until after those methods
;; would have been defined. Furthermore, at such a time as
;; once that FTYPE declaration has been produced, the
;; containing generic function should not allow addition of
;; any incompatibly specialized methods.
;;
;; In a manner, this may somewhat serve to extend of the
;; semantics of Common Lisp DEFSTRUCT accessors, onto a
;; domain of CLOS Standard Objects


;; Subsequent to the initialization of each (runtime) init,
;; next (for single-threaded dispatch - TBD), and exit function
;; for an application, each function - as such - may be stored
;; as to be accessed with a functional signature compatible with
;; the following source pattern.


;; ---- application condition handling

(defgeneric handler-dispatch (condition application))


;; ---- application restarts - continue restart

;; NB: The RESTART-BIND forms must be evaluated before any restart may
;; actually have been initialized.
;;
;; It may be assumed that in the following, a restart may be invoked
;; from within any one of:
;; - application runtime init or next (main) or exit lambda
;; - application condition handling lambda


;; TO DO: Define a reasonable default for each of the following methods
;; such that will serve to provide a normal restart-handling protocol
;; for modular applications.
;; i.e
#+NIL
(eval-when ()
  (compute-restart-lambda :main 'continue *application*)
  (compute-restart-lambda :interactive 'continue *application*)
  (compute-restart-lambda :report 'continue *application*)
  (compute-restart-lambda :test 'continue *application*)
  )


;; ---- application main exec

(defmacro controller-main ()
  (do-vector (*application*
              (controller-registered-applications %controller-singleton%)
              %controller-singleton%)
    (let ((%application-lambda%
           (app-runtime-next-lambda *application*)))
      (restart-bind ((continue
                      (restart-function :main 'continue *application*)
                       :interactive
                       (restart-function :interactive 'continue *application*)
                        :report
                       (restart-function :report 'continue *application*)
                       :test
                       (restart-function :test 'continue *application*)))
        (handler-bind ((condition (lambda (cdn)
                                    (handler-dispatch cdn app))))
          (funcall %application-lambda%))))))
