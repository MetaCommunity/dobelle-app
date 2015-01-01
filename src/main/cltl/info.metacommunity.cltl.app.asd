;; info.metacommunity.cltl.app.asd			-*-lisp-*-

(in-package #:asdf-user)

(defsystem #:info.metacommunity.cltl.app
  :description 
  "API for application initialization and messaging in Common Lisp"
  :version "1.0"
  :homepage "https://github.com/MetaCommunity/dobelle-app"
  :license "https://github.com/MetaCommunity/dobelle-app/blob/master/LICENSE"
  
  :depends-on (#:info.metacommunity.cltl.utils)

  :components 
  ((:file "app-package")
   
   (:file "label"
          :depends-on
          ("app-package"
           ))

   (:file "notify"
          :depends-on 
          ("app-package"
           ))
   ))
