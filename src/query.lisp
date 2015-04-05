(in-package :cl-user)
(defpackage cl-webapp-sample.query
  (:use :cl)
  (:import-from :cl-webapp-sample.query.post
                :get-posts
                :post-posts)
  (:export :get-posts
           :post-posts))
(in-package :cl-webapp-sample.query)
