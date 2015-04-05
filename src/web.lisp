(in-package :cl-user)
(defpackage cl-webapp-sample.web
  (:use :cl
        :caveman2
        :cl-webapp-sample.config
        :cl-webapp-sample.view
        :cl-webapp-sample.query
;        :cl-webapp-sample.request
;        :cl-webapp-sample.db
;        :datafly
;        :sxql
        )
  (:export :*web*))
(in-package :cl-webapp-sample.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

@route GET "/posts"
(defun api-get-posts ()
  (render-json (get-posts)))

@route POST "/posts"
(defun api-post-posts ()
  (let ((params (get-post-params)))
    (let ((name (cdr (assoc "name" params :test #'string=)))
          (text (cdr (assoc "text" params :test #'string=))))
      (post-posts :name name :text text))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))

;;
;; Helpers
(defun get-post-params ()
  (body-parameter *request*))
