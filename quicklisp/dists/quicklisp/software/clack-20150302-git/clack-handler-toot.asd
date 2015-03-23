#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Handler.Toot - Clack handler for Toot.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
  Author: κeen
|#

(in-package :cl-user)
(defpackage :clack-handler-toot-asd
  (:use :cl :asdf))
(in-package :clack-handler-toot-asd)

(defsystem clack-handler-toot
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :toot
               :split-sequence
               :cl-ppcre)
  :components ((:file "src/core/handler/toot"))
  :description "Clack handler for Toot.")
