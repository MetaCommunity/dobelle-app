;; info.metacommunity.cltl.app.asd			-*-lisp-*-

(in-package #:asdf-user)

(defsystem #:ltp-dobelle-app
  :description
  "API for application initialization and messaging in Common Lisp"
  :version "1.0"
  :homepage "https://github.com/MetaCommunity/dobelle-app"
  :license "https://github.com/MetaCommunity/dobelle-app/blob/master/LICENSE"

  :depends-on (#:ltp-common #+FIXME #:ltp-common-index)

  :components
  ((:file "app-package")

   (:file "notify"
          :depends-on
          ("app-package"
           ))
   ))
