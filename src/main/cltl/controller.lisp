;; NB: May be moved into a package ltp/dobelle/app
;; w/ symbols subsq renamed s/application-//


;; ----

(defgeneric controller-registered-applications (controller))

(defsingleton application-controller ()
  (...))


;; --

(defgeneric application-runtime-init-lambda (application))
(defgeneric application-runtime-next-lambda (application))
(defgeneric application-runtime-exit-lambda (application))

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
(defgeneric application-runtime-init-function (application))
(defgeneric application-runtime-next-function (application))
(defgeneric application-runtime-exit-function (application))
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

;; TO DO: Define a reasonable default for each of the following generic
;; functions, such that will serve to provide a normal restart-handling
;; protocol for modular applications. (DEFINE-RESTART-HANDLER ?)

(defgeneric application-continue-restart-main-lambda (TBD))
(defgeneric application-continue-restart-interactive-lambda (TBD))
(defgeneric application-continue-restart-report-lambda (TBD))
(defgeneric application-continue-restart-test-lambda (TBD))


;; ---- application main exec

(do-vector (*application*
            (controller-registered-applications %controller-singleton%)
            %controller-singleton%)
  (let ((%application-lambda%
         (application-runtime-next-lambda *application*)))
    (restart-bind ((continue
                    (application-continue-restart-main-lambda *application*)
                     :interactive
                     (application-continue-restart-interactive-lambda *application*)
                     :report
                     (application-continue-restart-report-lambda *application*)
                     :test
                     (application-continue-restart-test-lambda *application*)))
      (handler-bind ((condition (lambda (cdn)
                                  (handler-dispatch cdn app))))
        (funcall %application-lambda%)))))
