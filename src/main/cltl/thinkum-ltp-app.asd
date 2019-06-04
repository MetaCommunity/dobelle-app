;; info.metacommunity.cltl.app.asd			-*-lisp-*-

(in-package #:asdf-user) ;; NB: provided by ASDF

(defsystem #:thinkum-ltp-app
  :description 
  "API for application initialization and messaging in Common Lisp"
  :version "1.0"
   ;; FIXME update source availability
  :homepage "https://github.com/MetaCommunity/dobelle-app"
  :license "https://github.com/MetaCommunity/dobelle-app/blob/master/LICENSE"
  
  :depends-on (#:ltp-utils
		 ;; NB Of course, the folowing would be ignored if bordeaux-thread is not loaded before this sysdef is evaluated
               #+bordeaux-threads #:bordeaux-threads ;; towards a non-nill CONTEXT for NOTIFY
               )

  :components 
  ((:file "app-package")
   
   (:file "notify"
          :depends-on 
          ("app-package"
           ))
   ))
