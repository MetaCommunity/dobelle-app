;; info.metacommunity.cltl.application.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:application-system
    (:use #:asdf #:cl)))

(in-package #:application-system)


(defsystem #:info.metacommunity.cltl.application
  :description 
  "API for application initialization and messaging"
  :version "1.0"
  ;; :homepage "https://github.com/MetaCommunity/mci-cltl-application"
  ;; :license "https://github.com/MetaCommunity/mci-cltl-application/blob/master/LICENSE"
  
  :depends-on (#:info.metacommunity.cltl.utils)

  :components 
  ((:file "app-package")

   (:file "notify"
          :depends-on 
          ("app-package"
           ))
   ))

 
