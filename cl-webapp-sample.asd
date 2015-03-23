(in-package :cl-user)
(defpackage cl-webapp-sample-asd
  (:use :cl :asdf))
(in-package :cl-webapp-sample-asd)

(defsystem cl-webapp-sample
  :version "0.1"
  :author "Kei Nakamura"
  :license ""
  :depends-on (:clack
               :caveman2
               :envy
               :cl-ppcre

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (load-op cl-webapp-sample-test))))