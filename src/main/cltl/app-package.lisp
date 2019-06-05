;; app-package.lisp - dobelle-app-notify

(in-package #:ltp/common)

(defpackage #:ltp/dobelle/app
  (:nicknames #:mcicl.app
              ;; FIXME [Deprecated] - Both
              #:info.metacommunity.cltl.application)
  (:use #:ltp/common
        #+FIXME #:ltp/common/mop
        #+FIXME #:ltp/common/index
        #:cl)
  (:export

   #+NIL #:label
   #+NIL #:labeled-object
   #+NIL #:princ-label
   ;; ^ use MCICL.UTILS:PRETTY-PRINTABLE-OBJECT instead

   #:current-application

   #:format-condition

   #:encapsulated-condition
   #:encapsulated-condition-object

   #:application-condition
   #:application-condition-application

   #:application-error
   #:application-warning

   #:notify
   ))
