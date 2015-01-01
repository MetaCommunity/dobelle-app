;; app-package.lisp - dobelle-app-notify

(defpackage #:info.metacommunity.cltl.application
  (:nicknames #:mcicl.app)
  (:use #:info.metacommunity.cltl.utils #:cl)
  (:export 

   #+NIL #:label
   #+NIL #:labeled-object
   #+NIL #:princ-label
   ;; ^ use UTILS:PRETTY-PRINTABLE-OBJECT instead

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
