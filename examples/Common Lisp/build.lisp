;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Application build script
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :split-sequence)
  (require :zeromq))

(defpackage #:zguide.build
  (:nicknames #:build)
  (:use #:cl)
  (:export
   #:build))

(in-package :zguide.build)

(defun build (app-name)
  (load (compile-file "zhelpers"))
  (load (compile-file app-name))

  (let ((app-entry (find-symbol "MAIN" (string-upcase app-name))))
    #+sbcl (sb-ext:save-lisp-and-die app-name :executable t :toplevel app-entry)
    #+ccl (ccl:save-application app-name :prepend-kernel t :toplevel-function app-entry)))
