(in-package :cl-user)
(defpackage cl-webapp-sample.query
  (:use :cl)
  (:import-from :integral
                :*auto-migration-mode*
                :connect-toplevel)
  (:import-from :cl-webapp-sample.query.post
                :get-posts
                :post-posts)
  (:export :get-posts
           :post-posts))
(in-package :cl-webapp-sample.query)

(setf *auto-migration-mode* t)
(connect-toplevel :mysql
                    :database-name "mytest"
                    :username "mysql"
                    :password "mysql")
