;; app-package.lisp - dobelle-app-notify

(defpackage #:info.metacommunity.cltl.application
  (:nicknames #:application)
  (:use #:info.metacommunity.cltl.util #:cl)
  (:export 
   
   #:format-condition 

   #:encapsulated-condition
   #:encapsulated-condition-object
   
   #:application-condition
   #:application-condition-application

   #:application-error
   #:application-warning
   
   #:notify
   ))
