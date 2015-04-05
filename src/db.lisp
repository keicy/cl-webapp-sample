(in-package :cl-user)
(defpackage cl-webapp-sample.db
  (:use :cl)
  (:import-from :cl-webapp-sample.config
                :config)
  (:import-from :datafly
                :*connection*
                :connect-cached)
  (:import-from :integral
                :*auto-migration-mode*
                :connect-toplevel)
  (:export :connection-settings
           :db
           :with-connection))
(in-package :cl-webapp-sample.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

;;;;kei 

(setf integral:*auto-migration-mode* t)
(connect-toplevel :mysql
                    :database-name "mytest"
                    :username "mysql"
                    :password "mysql")
