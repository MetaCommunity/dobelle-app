
(in-package #:cl-user)

(asdf:operate 'asdf:load-op '#:iolib/os)

(defvar %pkg/cmd% "/usr/sbin/pkg")

(deftype %fd () ;; FIXME: find FD grovely-type
  'unsigned-byte) 

(defstruct pty
  ;; NB:
  ;; (iolib/os::setup-slave-pty t)
  ;; => FD, PTS_PATH
  (stream (make-instance 'iolib/os:tty-stream)
          :type iolib/streams:dual-channel-gray-stream)
  (fd 0 :type %fd)
  (pts-path "" :type simple-string))

(defclass os-process (iolib/os:process)
  ;; NB:
  ;;  iolib/os:process-stdin
  ;;  iolib/os:process-stout
  ;;  iolib/os:process-stderr
  ;;  iolib/os:process-pty
  ;;  iolib/os:process-pid
  ()
  )

(defclass executor-class (standard-class)
  ())

(defclass os-cmd (os-process)
  ((pathname
    :initarg :name
    :type pathname
    :reader os-cmd-pathname)
   (arg0
    :initarg :arg0
    :type simple-string
    :reader os-cmd-arg0)
   (args
    :initarg :args
    :type sequence
    :reader os-cmd-args)
   (controlling-thread
    :type bt:thread
    :accessor os-cmd-controlling-thread
    ))
  (:metaclass executor-class)
  )

(defconstant %pathname-name-type-separator% #\.)

(defun pathname-name-type (pathname) ;; util
  (with-output-to-string (s)
    (princ (pathname-name pathname) s)
    (let ((type (pathname-type pathname)))
      (when type
        (write-char %pathname-name-type-separator% s)
        (princ type s)))))

;; (pathname-name-type (pathname "/usr/local/bin/b.sh"))
;; => "b.sh"
;; (pathname-name-type (pathname "/usr/local/bin/a.b.sh"))
;; => "a.b.sh"
;; (pathname-name-type (pathname "/usr/local/bin/b"))
;; => "b"


(defmethod shared-initialize :around ((instance os-cmd) slots
                                      &rest initargs
                                      &key pathname arg0
                                        &allow-other-keys)
  (let (args-updated-p)
    (when (and pathname (not arg0))
      ;; derive :arg0 from :pathname
      (setf (getf inittargs :arg0)
            (pathname-name-type pathname))
      (setq args-udpated-p t))

    (cond
      (args-updated-p
       (apply #'call-next-method instance slots initargs))
      (t (call-next-method)))))


(defgeneric run (executor)
  (:method ((executor os-cmd))
    ;; modeled after IOLBI/OS:CREATE-PROCESS
    (let ((cmd-inst (make-instance os-cmd)))
      
      (setf (cmd-process cmd-inst)
            (iolbi/os:create-process 
  
;; (defun make-null-output-stream (&key (element-type 'character)
;;                                   (external-format :default))
;;   (open "/dev/null" :direction :output
;;         :element-type element-type))


;; (defun make-null-input-stream (&key (element-type 'character)
;;                                   (external-format :default))
;;   (open "/dev/null" :direction :input
;;         :element-type element-type))


(defun pkg/query/name (name &optional (info-format "%o/%n"))
  (let ((args-list (list "query" "-e"
                         (format nil "%n = ~A" name)
                         info-format))
        (out (make-string-output-stream))
        (err (make-string-output-stream)))
    (iolib/os:create-process (cons %pkg/cmd% args-list)
                             ;; :environment ...
                             :stdin :null
                             :stdout out
                             :stderr err)
    (values (get-output-stream-string out)
            (get-output-stream-string err))))

                             


;; (pkg/query/name "ccl")
