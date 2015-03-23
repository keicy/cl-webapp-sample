(in-package :cl-user)
(defpackage cl-webapp-sample-test-asd
  (:use :cl :asdf))
(in-package :cl-webapp-sample-test-asd)

(defsystem cl-webapp-sample-test
  :author "Kei Nakamura"
  :license ""
  :depends-on (:cl-webapp-sample
               :prove)
  :components ((:module "t"
                :components
                ((:file "cl-webapp-sample"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
