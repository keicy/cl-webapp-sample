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
               :integral
               :sxql
               )
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "main" :depends-on ("config"))
                 (:file "web" :depends-on ("view" "query"))
                 (:file "view" :depends-on ("config"))
                 (:file "query" :depends-on ("query-modules"))
                 (:module "query-modules"
                  :pathname "query"
                  :components
                  ((:file "post"))))))
  :description ""
  :in-order-to ((test-op (load-op cl-webapp-sample-test))))
