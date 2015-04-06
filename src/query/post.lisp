(in-package :cl-user)
(defpackage cl-webapp-sample.query.post
  (:use :cl)
  (:import-from :integral
                :select-dao
                :save-dao
                :<dao-table-class>
                :where
                :order-by
                :group-by
                :limit
                :offset)
  (:export :get-posts
           :post-posts))
(in-package :cl-webapp-sample.query.post)

(defclass post ()
  ((id :type integer
       :primary-key t
       :auto-increment t)
   (name :type text
         :initarg :name)
   (text :type text
         :initarg :text))
  (:metaclass <dao-table-class>))

(defun get-posts ()
  (select-dao 'post))

(defun post-posts (&key name text)
  (let ((elm (make-instance 'post :name name :text text)))
    (save-dao elm)))

