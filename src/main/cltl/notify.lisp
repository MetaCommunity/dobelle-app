

(in-package #:application)


;;;; Trivial application notification framework for condition handling


(defclass application ()
  ())


(defvar %application% nil)

;; FIXME: Define an APPLICATION class representative of the Lisp image itself

(defun current-application ()
  (values %application%))

(defun (setf current-application) (new-value)
  ;; FIXME: Thread handling?
  (setf %application% new-value))


;; TO DO - w/ study onto POSIX
;; (defclass process ()
;;   ())
;;
;; #+POSIX
;; (defclass thread-process (local-process)
;;   (...))
;;
;; (defgeneric get-process-message-bus (process))
;;
;; (defclass clim-application (process) ...)
;;  ;; application-frame, etc.
;;
;; (defclass remote-process (process) ...)
 

(defgeneric format-condition (condition stream))


(define-condition encapsulated-condition ()
  ;; This class might be semantically similar to CELL-ERROR, but does
  ;; not include ERROR in its class precedence list
  ((object
    :initarg :condition
    :accessor encapsulated-condition-object)))

(define-condition application-condition (encapsulated-condition)
  ((application
    :initarg :application
    :initform %application%
    :reader application-condition-application)
   (application-context
    :initarg :application-context
    :initform nil
    :reader application-condition-application-context))
  (:report format-condition))

(defmethod format-condition ((condition application-condition)
                             (stream stream))
  ;; FIXME: #I18N
  (format stream "~<Encapsulated condition in context ~A within ~A:~> ~<~A~>"
          (application-condition-application-context condition)
          (application-condition-application condition)
          (encapsulated-condition-object condition)))

(define-condition application-error (error application-condition)
  ())

#+NIL
(handler-case ; instance test - application-error
    (simple-program-error "Ping ping ping ping ~s" (get-universal-time))
  (error (c)
    (error 'application-error 
           :application "Foo top level foo" 
           :condition c)))


(define-condition application-warning (warning application-condition)
  ())


(defgeneric notify (application context condition)
  ;; FIXME: Clarify usage of  CONTEXT argument
  ;;
  ;; e.g. for  CLIM-APP APPLICATION
  ;;           CONTEXT classes e.g
  ;;              WINDOW-CONTEXT
  ;;              SHELL-PROCESS-CONTEXT
  ;;              HOST-CONTEXT
  ;;              etc
  (:method ((application null) context (condition error))
    (error 'application-error
           :application nil
           :application-context context
           :condition condition))

  (:method ((application null) context (condition warning))
    (error 'application-warning
           :application nil
           :application-context context
           :condition condition))

  (:method ((application null) context (condition condition))
    (signal 'applicaiton-condition
            :application nil
            :application-context context
            :condition condition)))


;; NOTE/TODO: This system now dep. on [system Bordeaux Threads]
#+Bordeaux-Threads
(defun notify* (condition)
  ;; FIXME: 
  ;;
  ;; 1. Define a CLASS, THREAD, utilizing of Bordeaux-Threads'
  ;;    implementation-specific DEFTYPE interface for reference
  ;;
  ;;    [system TBD : MCI // process, container for subsequent items
  ;;    denoted here]
  ;;
  ;; 2. Specialize NOTIFY onto (CONTEXT THREAD)
  ;;
  ;; 3. Define a class, PROCESS, utilizing of OSICAT's POSIX interface
  ;; 
  ;; 4. Specialize NOTIFY onto (CONTEXT PROCESS)
  ;;
  ;; 5. Define a class, HOST, utilizing of AWS' EC2 API
  ;; 
  ;; 6. Define an interface for computing the {AWS messaging} identity
  ;;    for [a] {host administrator[s]} (cf. also AWS IAM) via the AWS
  ;;    API [Java]
  ;;
  ;; 7. Specialize NOTIFY onto HOST, such that a host administrator
  ;;    would be notified of the respective CONDITION in the specified
  ;;    APPLICATION at the effective "base" of the notification stack
  ;;
  ;; 8. Specify a clear policy that for a "Development server," the
  ;;    "host administrator" is a developer - as then in a context
  ;;    primarily of executive dissertations, if not
  ;;    entrepreneurialism
  ;;
  (notify (current-application) (bordeaux-threads:current-thread) condition))


#+NIL
(handler-case ; instance test - #'notify* and ... error method
    (simple-program-error "Ping ping ping ping ~s" (get-universal-time))
  (error (c)
    (notify* c)))
