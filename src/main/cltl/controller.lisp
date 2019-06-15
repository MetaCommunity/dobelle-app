;; controller.lisp - LTP Dobelle application-controller protocol

;; NB: May be moved into a package ltp/dobelle/app
;; w/ symbols subsq renamed s/app-//


#|

Remarks - API Design

- This API endeavors to develop some characteristics of a concept,
  broadly: Normal application definitions, for Common Lisp programs.

  Below, this concept - in a manner, an application pattern -- is
  represented insofar as for two generic usage cases in Common Lisp
  programming, principally:

    - Representation of Common Lisp Restarts and Condition Objects

    - Definition of a generic API for definition and management
      of an application runtime lifecycle - in a manner: Application
      initialization, application runtime event handling, and
      application termination.


  With regards to representations of restarts and condition objects, in
  Common Lisp programs, two generic usage cases are addressed below:

    - Interactive Common Lisp applications, represented here as vis a
      vis: User interaction with the implementation debugger - here,
      focusing on the implementation of a generic 'CONTINUE' restart, at
      a granularity of a genric application API

    - N!ointeractive Common Lisp applications, such as may provide any
      manner of intrinsic handling for Common Lisp conditions, in any
      arbitrary application usage cases. (NB: CERROR -> CONTINUE := FAIL)


  The concept of a generic API for applications lifecyle (definition and
  management) is developed - albeit in something of a rudimentary regard -
  towards applications in multi-threaded and single-threaded Common Lisp
  programming environments.


  A generic API is proposed for asynchronous dispatch onto multiple
  applications in single-threaded Common Lisp programming environments,
  such that might serve - in some limited way - as to resemble an
  application data flow for multi-threaded   applications.

  In this regard, an APPLICATION-CONTROLLER is developed, such that the
  implementation of the APPLICATION-CONTROLLER will differ as per
  whether the Common Lisp implementation is assumed to provide, or
  assumed to not provide an API for multi-threaded applications.


  It is assumed that any API for multi-threaded applications may be
  provided in a manner generally resembling POSIX threads.


--

  In a broad manner, the programming pattern - developed below - defines
  a separation between the Common Lisp runtime image and individual
  tasks, such that may be comprised of simultaneous threads or
  asynchronous call structures in, respectively, a multi-threaded or
  single-threaded programming environment.

--

  This API extends the SINGLETON MOP extension -- as developed in the
  Thinkum Labs Lisp Tools Project (LTP) -- extending the SINGLETON
  pattern as to define a generic enumerated type, in a manner such that
  any members of the enumerated type, or the enumerated type itself, may
  be used for principally static dispatching in Common Lisp method
  definitions.

  The macro DEFINE-SINGLETON-ENUM .... [TBD: Documentation]

--

  The macro CONTROLLER-MAIN provides a generic, pseudocode-like example
  of a top-level function for an APPLICATION-CONTROLLER. The source code
  of this macro, as such, may be subject to change in subsequent
  revisions of this soruce file.

|#

(in-package #:ltp/common)

(defpackage #:ltp/dobelle/app
  (:nicknames #:ltp.dobelle.app)
  (:use #:ltp/common/singleton
        #:ltp/common/mop
        #:ltp/common
        #:cl))


(in-package #:ltp/dobelle/app)



(eval-when ()
(define-singleton-enum retart-kind (kind)
  ()
  ((continue-restart-kind continue))
  (:descriptor name))
;; ^ subsq defines functions:
;; - REGISTER-RESTART-KIND
;; - FIND-RESTART-KIND
;; - REMOVE-RESTART-KIND
;; also defines singleton classes
;; - CONTINUE-RESTART-KIND


(define-singleton-enum restart-lambda-kind (kind)
  ()
  ((restart-main-lambda-kind :main)
   (restart-interactive-lambda-kind :interactive)
   (restart-report-lambda-kind :report)
   (restart-test-lamba-kind :test))
  (:descriptor name))
;; ^ subsq defines functions:
;; - REGISTER-RESTART-LAMBDA-KIND
;; - FIND-RESTART-LAMBDA-KIND
;; - REMOVE-RESTART-LAMBDA-KIND
;; also defines singleton classes:
;; - RESTART-MAIN-LAMBDA-KIND
;; - RESTART-INTERACTIVE-LAMBDA-KIND
;; - RESTART-REPORT-LAMBDA-KIND
;; - RESTART-TEST-LAMBA-KIND

)


;; --

(defgeneric kind-name (kind))
(defgeneric (setf kind-name) (new-name kind))

(defclass kind ()
  ((name
    :type symbol
    :accessor kind-nme
    :initarg :name)))


(defclass restart-kind (kind)
  ())

;; (defun register-restart-kind (kind))
;; (defun find-restart-kind (name &optional (noerr nil)))
;; (defun remove-restart-kind (kind))


(defsingleton continue-restart-kind (restart-kind)
  ()
  (:default-initargs :name 'continue))

;; (register-restart-kind 'continue-restart-kind)

(defclass lambda-kind (kind)
  ())

;; (defun register-lambda-kind (kind))
;; (defun find-lambda-kind (name &optional (noerr nil)))
;; (defun remove-lambda-kind (kind))


(defsingleton restart-main-lambda-kind (lambda-kind)
  ()
  (:default-initargs :name :main))

(defsingleton restart-interactive-lambda-kind (lambda-kind)
  ()
  (:default-initargs :name :interactive))

(defsingleton restart-report-lambda-kind (lambda-kind)
  ()
  (:default-initargs :name :report))

(defsingleton restart-test-lambda-kind (lambda-kind)
  ()
  (:default-initargs :name :test))

;; (register-lambda-kind ...) ....


(defgeneric compute-restart-lambda (lambda-kind restart-kind application)
  (:method ((lambda-kind symbol) (restart-kind symbol)
            (application application))
    (let ((lk-singleton (find-restart-lambda-kind lambda-kind))
          (rk-singleton (find-restart-kind restart-kind)))
      (compute-restart-lambda lk-singleton rk-singleton application))))


(defgeneric restart-function (lambda-kind restart-kind application)
(defgeneric (setf restart-function) (function lambda-kind restart-kind application))


;; ----

(defgeneric controller-registered-applications (controller))


(defclass app-controller ()
  ())

(defsingleton app-controller/single-thread (app-controller)
  ())

;; #+SOME-LISP-HACK
(defsingleton app-controller/multi-thread (app-controller)
  ())


;; --

(defclass application ()
  ())

(defgeneric app-runtime-init-lambda (application))
;; (defgeneric app-runtime-next-lambda (application))
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
