#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman.request
  (:use :cl
        :clack.request)
  (:shadow :<request>
           :make-request)
  (:export :request-method
           :script-name
           :path-info
           :server-name
           :server-port
           :server-protocol
           :request-uri
           :uri-scheme
           :remote-addr
           :remote-port
           :query-string
           :raw-body
           :content-length
           :content-type
           :clack-handler

           :referer
           :user-agent
           :uploads

           :securep
           :cookies
           :body-parameter
           :query-parameter
           :parameter))
(in-package :caveman.request)

(cl-syntax:use-syntax :annot)

@export
(defclass <request> (clack.request:<request>) ()
  (:documentation "Class for Caveman Request."))

@export
(defun make-request (req)
  "Construct a request instance."
  (apply #'make-instance '<request>
         :allow-other-keys t
         :raw-body (shared-raw-body req)
         req))

(doc:start)

@doc:NAME "
Caveman.Request - Request class for Caveman.
"

@doc:DESCRIPTION "
Caveman.Request is a request class for Caveman. Caveman creates a `<request>' instance for each request and bind it to `*request*'.
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Request
"
